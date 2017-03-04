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
import java.util.ArrayList;

/**
 * Watches a specific folder for changes and executes handlers for updating the
 * list
 *
 */
public class FileWatcher extends Thread {

	private String directoryPath;
	private String virtualRoot;
	private Boolean watchForNewFiles;
	private FileWatcherController controller;
	public WatchService watcher;
	public WatchKey key;

	public FileWatcher(String path, Boolean watchForNewFiles, FileWatcherController controller, String virtualRoot) {
		super();

		this.directoryPath = addSlashToFileNameIfNecessary(path);
		this.watchForNewFiles = watchForNewFiles;
		this.controller = controller;
		this.virtualRoot = virtualRoot;
	}

	/**
	 * Folders may not have '/' at the end, so fix that
	 * 
	 * @param fileName
	 * @return
	 */
	private String addSlashToFileNameIfNecessary(String fileName) {
		if (fileName.charAt(fileName.length() - 1) != File.separatorChar) {
			return fileName + File.separatorChar;
		} else {
			return fileName;
		}
	}
	
	public void stopWatching() {
		if (watcher != null) {
			try {
				System.out.println("Watcher stop called");
				if (key != null)
					key.cancel();
				watcher.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
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
			watcher = FileSystems.getDefault().newWatchService();

			// Register
			Path dir = Paths.get(directoryPath);
			dir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY);

			while (!Thread.currentThread().isInterrupted()) {
				
				System.out.println("Start new wait round: " + directoryPath);
				
				try {
					// wait for a key to be available
					key = watcher.take();
				} catch (InterruptedException ex) {
					stopWatching();
					Thread.currentThread().interrupt();
					System.out.println("INTERRUPTED");
					break;
				}

				// Lock semaphore
				//controller.lock.lock();

				for (WatchEvent<?> event : key.pollEvents()) {

					// get event type
					WatchEvent.Kind<?> eventType = event.kind();

					// get file name
					@SuppressWarnings("unchecked")
					WatchEvent<Path> ev = (WatchEvent<Path>) event;
					Path fileName = ev.context();

					String fullPath = directoryPath + fileName.toString();

					File newFile = new File(fullPath);

					System.out.println("FileWatcher detected event for: "+fullPath);
					System.out.println("FileWatcher instance: " + directoryPath);
					System.out.println(eventType.name() + ": " + fullPath);

					// New file or directory was created -> Add to list
					if (watchForNewFiles && eventType == ENTRY_CREATE) {

						System.out.println("..New file was created -> Add to list: " + newFile.getPath());

						// New directory was created
						if (newFile.isDirectory()) {
							controller.watchDirectoryRecursively(addSlashToFileNameIfNecessary(newFile.getPath()), virtualRoot);
						}
						
						// New file was created
						else {
							controller.addFileToLists(fullPath, virtualRoot);
						}
						
					} else {
						
						FileWatcherTreeNode node = controller.currentFiles.getNodeByFileName(fullPath);

						// File or folder was deleted and in our list
						if (node != null && (eventType == OVERFLOW || eventType == ENTRY_DELETE)) {
							
							System.out.println("FileWatcher: Detected DEL event for: "+fullPath);

							// DEL
							// File is not a folder
							if (node.fileInfo != null) {
								fileWasDeleted(node);
							}
							
							// File is a folder
							else {
								if (node.hasChildren()) {
									folderWasDeleted(addSlashToFileNameIfNecessary(fullPath));
								} else {
									System.out.println("Folder has no subfolders - Only remove from internal dir list");
									controller.watchedInternalDirectories.remove(addSlashToFileNameIfNecessary(fullPath));
								}
							}
						}
						
						// File was modified
						// We are only interested in modified files, not folders
						if (node != null && node.fileInfo != null && eventType == ENTRY_MODIFY) {
							fileWasModified(fullPath, node);
						}
					}
				}

				// Reset key
				boolean valid = key.reset();
				if (!valid) {
					break;
				}

				//controller.lock.unlock();

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
		System.out.println("FileWatcher: Deleted file seems to be a folder");
		
		// Check if folder is in our list
		System.out.println("Check if folder is in our list: " + fullPath);
		if (controller.watchedInternalDirectories.contains(fullPath)) {

			File probablyDeletedFolder = new File(fullPath);

			// Check if folder does still exist
			System.out.println("FileWatcher: Check if folder does still exist");
			
			if (!probablyDeletedFolder.exists()) {
				controller.unwatchDirectory(fullPath);

				// Delete all sub folders from watchtedInternalDirectories
				System.out.println("Delete all sub folders from watchedInternalDirectories");
				ArrayList<String> removeList = new ArrayList<String>();
				for (String subfolder : controller.watchedInternalDirectories) {
					if (subfolder.contains(fullPath)) {
						removeList.add(subfolder);
						controller.stopWatchThread(subfolder);
						System.out.println("Subfolder added to delete list: " + subfolder);
					}
				}
				
				controller.watchedInternalDirectories.removeAll(removeList);
				controller.deleteFolderFromLists(fullPath);
				
			}
												
		}
	}

	private void fileWasDeleted(FileWatcherTreeNode node) {
		System.out.println("FileWatcher: Deleted file is not a folder ("+node.fileInfo.fileName+")");

		// Check if that file was really deleted
		File probablyDeletedFile = new File(node.fileInfo.fileName);

		// File does not exist anymore -> It was deleted
		if (!probablyDeletedFile.exists()) {
			controller.deleteFileFromLists(node.fileInfo);

			System.out.println("File was deleted: " + node.fileInfo.fileName);

			// Unwatch Directory if directory does not
			// exist anymore
			/*
			 * File parentDir = new
			 * File(fileFromList.parentDirectory); if
			 * (!parentDir.exists())
			 * controller.unwatchDirectory(directoryPath
			 * );
			 */
		}
	}

	private void fileWasModified(String fullPath, FileWatcherTreeNode node) throws Exception {
		System.out.println("FileWatcher: Detected MODIFY event");

		// DEL
		controller.deleteFileFromLists(node.fileInfo);

		// ADD
		controller.addFileToLists(fullPath, virtualRoot);
	}

}
