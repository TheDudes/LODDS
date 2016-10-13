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
 * Watches a specific folder for changes and executes handlers for updating the list
 *
 */
public class FileWatcher implements Runnable {

	private String directoryPath;
	private Boolean watchForNewFiles;
	private FileWatcherController controller;
	
	public FileWatcher(String path, Boolean watchForNewFiles, FileWatcherController controller) {
		super();
		
        // Sometimes folders don't have / at the end, so fix that
        if (!path.substring(path.length() - 1).equals("/")) {
        		 path = path+"/";
        }
       
		this.directoryPath = path;
		this.watchForNewFiles = watchForNewFiles;
		this.controller = controller;
	}
	
	/**
	 * Watches a directory for changes and if changes are detected
	 * it checks if the changed file is in our file list 
	 * or if its a recursively watched directory it monitors new files as well
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
					    return;
					}
					
					 
					// Lock semaphore
					// FileWatcherController.semaphore.acquire();
					controller.lock.lock();
					
					for (WatchEvent<?> event : key.pollEvents()) {

					        // get event type
					        WatchEvent.Kind<?> eventType = event.kind();

					        // get file name
					        @SuppressWarnings("unchecked")
					        WatchEvent<Path> ev = (WatchEvent<Path>) event;
					        Path fileName = ev.context();
					        String fullPath = directoryPath+fileName.toString();
					        FileInfo newFileInfo = null;
					        
					        File newFile = new File(fullPath);
					        
					        System.out.println(fullPath);
					        
					    	if (newFile.exists() && !newFile.isDirectory()) {
						        newFileInfo = new FileInfo(fullPath);
					    	} 

					        System.out.println("FileWatcher: "+directoryPath);
					        System.out.println(eventType.name() + ": " + fullPath);

					        FileInfo fileFromList = null;
					        
					        // New file was created -> Add to list
					        if (watchForNewFiles && eventType == ENTRY_CREATE) {
					        	
					        	System.out.println("..New file was created -> Add to list: "+newFile.getPath());
					        	
					        	if (newFile.isDirectory()) {
					        		// controller.watchDirectory(directoryPath, true);
					        		controller.watchDirectoryRecursively(newFile.getPath());
					        	} else {
					        		
					        		System.out.println("..File is not a directory");
							        newFileInfo = new FileInfo(fullPath);
					        		fileFromList = controller.getWatchedFileFromListByHash(newFileInfo.checksum);

					        		// Add file to watchList if its not already inside
						        	if (!controller.currentFilesListFileNameAsKey.containsKey(fullPath)) {
						        		System.out.println("..file is not in list so its added");
						        		controller.watchFile(fullPath, false);
						        		
						        	} else {
						        		System.out.println("..file is already in list so its not added");
						        	}
					        	}
					        } else {
					        	fileFromList = controller.getWatchedFileFromListByFileName(fullPath);
					        	
						        	// File or folder was deleted
							        if (eventType == OVERFLOW || eventType == ENTRY_DELETE) {

							        	// DEL
							        	// We don't know if a directory or file was deleted
							        	
							        	// We found a file in our list that was named like this
							        	if (fileFromList != null) { 
							        		
							        		// Check if that file was really deleted
							        		File probablyDeletedFile = new File(fileFromList.fileName);
							        		
							        		// File does not exist anymore -> It was deleted
							        		if (!probablyDeletedFile.exists()) {
									        	controller.deleteFileFromLists(fileFromList);
									        	
									        	System.out.println("File del, parent dir: "+fileFromList.parentDirectory);
									        	
									        	// Unwatch Directory if directory does not exist anymore
									        	/*File parentDir = new File(fileFromList.parentDirectory);
									        	if (!parentDir.exists())
									        		controller.unwatchDirectory(directoryPath);   */
							        		}
							        		
							        	} else {
							        		
							        	// File is not in list, so it could be a folder
							        	System.out.println("File is not in list, so it could be a folder");
							        	
							        		// Check if folder is in our list
							        		System.out.println("Check if folder is in our list: "+fullPath);
							        		if (controller.watchedInternalDirectories.contains(fullPath) || controller.watchedInternalDirectories.contains(fullPath+"/")) {
							        			
							        			File probablyDeletedFolder = new File(fullPath);
							        			
							        			// Check if folder does still exist
							        			System.out.println("Check if folder does still exist");
							        			if (!probablyDeletedFolder.exists()) {
							        				controller.watchedInternalDirectories.remove(fullPath);
							        				controller.watchedInternalDirectories.remove(fullPath+"/");
							        				
										        	// Delete all sub folders from watchtedInternalDirectories
							        				System.out.println("Delete all sub folders from watchtedInternalDirectories");
									        		for (String subfolder:controller.watchedInternalDirectories) {
									        			if (subfolder.contains(fullPath)) {
									        				controller.watchedInternalDirectories.remove(subfolder);
									        				System.out.println("Subfolder deleted: "+subfolder);
									        			}
									        		}
									        		
									        		// Delete all files from that folder
							        			}
							        		}

							        	}
							        	
							        // File was modified
							        } else if (fileFromList != null && eventType == ENTRY_MODIFY) {
							        	
							        	System.out.println("..File was modified");

							            // DEL
							        	controller.deleteFileFromLists(fileFromList);
							        	
							        	// ADD
							        	controller.addFileToLists(fullPath);

							        }
							        
						        
					        }
					        

					    }	
					 
					 	// FileWatcherController.semaphore.release();
					 	controller.lock.unlock();
					 
					    // Reset key
					    boolean valid = key.reset();
					    if (!valid) {
					        break;
					    }

				}


		}
		catch (IOException ex) {
	            System.err.println(ex);
	  } catch (NoSuchAlgorithmException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
