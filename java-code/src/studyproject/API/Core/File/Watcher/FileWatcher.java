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

import studyproject.API.Core.File.FileInfo;

/**
 * Watches a specific folder for changes and executes handlers for updating the
 * list
 *
 */
public class FileWatcher implements Runnable {

	private String directoryPath;
	private String virtualRoot;
	private Boolean watchForNewFiles;
	private FileWatcherController controller;

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
		if (!fileName.substring(fileName.length() - 1).equals("/")) {
			return fileName + "/";
		} else {
			return fileName;
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
			WatchService watcher = FileSystems.getDefault().newWatchService();

			// Register
			Path dir = Paths.get(directoryPath);
			dir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY);

			while (true) {

				WatchKey key;
				try {
					// wait for a key to be available
					key = watcher.take();
				} catch (InterruptedException ex) {
					break;
				}

				// Lock semaphore
				controller.lock.lock();

				for (WatchEvent<?> event : key.pollEvents()) {

					// get event type
					WatchEvent.Kind<?> eventType = event.kind();

					// get file name
					@SuppressWarnings("unchecked")
					WatchEvent<Path> ev = (WatchEvent<Path>) event;
					Path fileName = ev.context();
					String fullPath = directoryPath + fileName.toString();
					FileInfo newFileInfo = null;

					File newFile = new File(fullPath);

					System.out.println("FileWatcher detected event for: "+fullPath);

					if (newFile.exists() && !newFile.isDirectory()) {
						newFileInfo = new FileInfo(fullPath, virtualRoot);
					}

					System.out.println("FileWatcher instance: " + directoryPath);
					System.out.println(eventType.name() + ": " + fullPath);

					FileInfo fileFromList = null;

					// New file was created -> Add to list
					if (watchForNewFiles && eventType == ENTRY_CREATE) {

						System.out.println("..New file was created -> Add to list: " + newFile.getPath());

						// New directory was created
						if (newFile.isDirectory()) {
							controller.watchDirectoryRecursively(newFile.getPath(), virtualRoot);
						}
						
						// New file was created
						else {

							newFileInfo = new FileInfo(fullPath, virtualRoot);
							fileFromList = controller.getWatchedFileFromListByHash(newFileInfo.checksum);
							controller.addFileToLists(fullPath, virtualRoot);
						}
					} else {
						// fileFromList = controller.getWatchedFileFromListByFileName(fullPath);
						
						FileWatcherTreeNode node = controller.currentFiles.getNodeByFileName(fullPath);

						// File or folder was deleted and in our list
						if (node != null && (eventType == OVERFLOW || eventType == ENTRY_DELETE)) {
							
							System.out.println("FileWatcher: Detected DEL event for: "+fullPath);

							// DEL
							// File is not a folder
							if (node.fileInfo != null) {
								
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
							
							// File is a folder
							else {

								System.out.println("FileWatcher: Deleted file seems to be a folder");
								
								// Check if folder is in our list
								System.out.println("Check if folder is in our list: " + fullPath);
								if (controller.watchedInternalDirectories.contains(fullPath)
										|| controller.watchedInternalDirectories.contains(fullPath + "/")) {

									File probablyDeletedFolder = new File(fullPath);

									// Check if folder does still exist
									System.out.println("FileWatcher: Check if folder does still exist");
									
									if (!probablyDeletedFolder.exists()) {
										controller.watchedInternalDirectories.remove(fullPath);
										controller.watchedInternalDirectories.remove(fullPath + "/");

										// Delete all sub folders from
										// watchtedInternalDirectories
										System.out.println("Delete all sub folders from watchtedInternalDirectories");
										for (String subfolder : controller.watchedInternalDirectories) {
											if (subfolder.contains(fullPath)) {
												controller.watchedInternalDirectories.remove(subfolder);
												System.out.println("Subfolder deleted: " + subfolder);
											}
										}
										
										// TODO
										controller.deleteFolderFromLists(fullPath);
										
									}
																		
								}

							}
	 
						}
						
						// File was modified
						// We are only interested in modified files, not folders
						if (node != null && node.fileInfo != null && eventType == ENTRY_MODIFY) {

							System.out.println("FileWatcher: Detected MODIFY event");

							// DEL
							controller.deleteFileFromLists(node.fileInfo);

							// ADD
							controller.addFileToLists(fullPath, virtualRoot);

						}

					}

				}

				// Reset key
				boolean valid = key.reset();
				if (!valid) {
					break;
				}

				// FileWatcherController.semaphore.release();
				controller.lock.unlock();

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

}
