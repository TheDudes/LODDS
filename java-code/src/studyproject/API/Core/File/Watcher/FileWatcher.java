package studyproject.API.Core.File.Watcher;

import static java.nio.file.StandardWatchEventKinds.ENTRY_CREATE;
import static java.nio.file.StandardWatchEventKinds.ENTRY_DELETE;
import static java.nio.file.StandardWatchEventKinds.ENTRY_MODIFY;
import static java.nio.file.StandardWatchEventKinds.OVERFLOW;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

/**
 * Watches a specific folder for changes and executes handlers for updating the
 * list
 *
 */
public class FileWatcher extends Thread {

	Map<String, WatchedFolder> directoryKeys = new HashMap<String, WatchedFolder>();
	private FileWatcherController controller;
	public WatchService watcher;

	public FileWatcher(FileWatcherController controller) throws IOException {
		super();
		this.controller = controller;
		this.watcher = FileSystems.getDefault().newWatchService();
	}

	public void stopWatching(String directory) {
		System.out.println("stopWatching: " + directory);
		System.out.println(this.directoryKeys.keySet());
		WatchedFolder wf = this.directoryKeys.get(directory);
		if (wf != null)
			wf.key.cancel();
		this.directoryKeys.remove(directory);
	}

	public void registerDir(String path, String virtualRoot, Boolean watchForNewFiles) {
		path = controller.addSlashToFileNameIfNecessary(path);
		virtualRoot = controller.addSlashToFileNameIfNecessary(virtualRoot);

		Path dir = Paths.get(path);
		try {
			WatchKey watchKey = dir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY);
			WatchedFolder wf = new WatchedFolder(path, virtualRoot, watchKey, watchForNewFiles);
			directoryKeys.put(path, wf);
			System.out.println("Registered new dir: " + path);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private String getDirectoryByWatchKey(WatchKey watchKey) throws Exception {
		return getWatchedFolderByWatchKey(watchKey).fullPath;
	}

	private WatchedFolder getWatchedFolderByWatchKey(WatchKey wkey) {
		for (Entry<String, WatchedFolder> entry : this.directoryKeys.entrySet()) {
			if (entry.getValue().key == wkey)
				return entry.getValue();
		}
		return null;
	}

	/**
	 * Watches a directory for changes and if changes are detected it checks if
	 * the changed file is in our file list or if its a recursively watched
	 * directory it monitors new files as well
	 * 
	 * @param directoryPath
	 */
	public void run() {
		try {

			while (!Thread.currentThread().isInterrupted()) {
				WatchKey key;

				try {
					// wait for a key to be available
					key = watcher.take();

				} catch (InterruptedException ex) {
					// stopWatching();
					Thread.currentThread().interrupt();
					break;
				}

				for (WatchEvent<?> event : key.pollEvents()) {

					// get event type
					WatchEvent.Kind<?> eventType = event.kind();

					// get file name
					@SuppressWarnings("unchecked")
					WatchEvent<Path> ev = (WatchEvent<Path>) event;
					Path fileName = ev.context();

					String fullPath = getDirectoryByWatchKey(key) + fileName.toString();

					File newFile = new File(fullPath);

					System.out.println("FileWatcher detected event for:" + fullPath);
					// System.out.println(eventType.name() + ": " + fullPath);

					// New file or directory was created -> Add to list
					if (getWatchedFolderByWatchKey(key).watchForNewFiles && eventType == ENTRY_CREATE) {

						// System.out.println("..New file was created -> Add to
						// list: " + newFile.getPath());

						// New directory was created
						if (newFile.isDirectory()) {
							controller.watchDirectoryRecursively(
									controller.addSlashToFileNameIfNecessary(newFile.getPath()),
									getWatchedFolderByWatchKey(key).virtualRoot);
						}

						// New file was created
						else {
							controller.addFileToLists(fullPath, getWatchedFolderByWatchKey(key).virtualRoot);
						}

					} else {

						FileWatcherTreeNode node = controller.currentFiles.getNodeByFileName(fullPath);

						// System.out.println("#1.1 FileWatcher: Detected DEL
						// event for: "+fullPath);

						// File or folder was deleted and in our list
						if (node != null && (eventType == OVERFLOW || eventType == ENTRY_DELETE)) {

							System.out.println("FileWatcher: Detected DEL event for: " + fullPath);

							// DEL
							// File is not a folder
							if (node.fileInfo != null) {
								fileWasDeleted(node);
							}

							// File is a folder
							else {
								if (node.hasChildren()) {
									folderWasDeleted(controller.addSlashToFileNameIfNecessary(fullPath));
								} else {
									// System.out.println("Folder has no
									// subfolders - Only remove from internal
									// dir list");
									controller.watchedInternalDirectories
											.remove(controller.addSlashToFileNameIfNecessary(fullPath));
								}
							}
						}

						// File was modified
						// We are only interested in modified files, not folders
						if (node != null && node.fileInfo != null && eventType == ENTRY_MODIFY) {
							fileWasModified(fullPath, node, key);
						}
					}
				}

				// Reset key
				key.reset();

			}

		} catch (IOException ex) {
			System.err.println(ex);
		} catch (NoSuchAlgorithmException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private void folderWasDeleted(String fullPath) {
		System.out.println("FileWatcher: Deleted file seems to be a folder: " + fullPath);

		// Check if folder is in our list
		// System.out.println("Check if folder is in our list: " + fullPath);
		if (controller.watchedInternalDirectories.contains(fullPath)) {

			File probablyDeletedFolder = new File(fullPath);

			// Check if folder does still exist
			// System.out.println("FileWatcher: Check if folder does still
			// exist");

			if (!probablyDeletedFolder.exists()) {
				controller.unwatchDirectory(fullPath);
			}

		}
	}

	private void fileWasDeleted(FileWatcherTreeNode node) {
		// System.out.println("FileWatcher: Deleted file is not a folder
		// ("+node.fileInfo.fileName+")");

		// Check if that file was really deleted
		File probablyDeletedFile = new File(node.fileInfo.fileName);

		// File does not exist anymore -> It was deleted
		if (!probablyDeletedFile.exists()) {
			controller.deleteFileFromLists(node.fileInfo);
		}
	}

	private void fileWasModified(String fullPath, FileWatcherTreeNode node, WatchKey key) throws Exception {
		// System.out.println("FileWatcher: Detected MODIFY event");

		// DEL
		controller.deleteFileFromLists(node.fileInfo);

		// ADD
		controller.addFileToLists(fullPath, getWatchedFolderByWatchKey(key).virtualRoot);
	}

}
