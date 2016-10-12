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

	private String path;
	private Boolean watchForNewFiles;
	private FileWatcherController controller;
	
	public FileWatcher(String path, Boolean watchForNewFiles, FileWatcherController controller) {
		super();
		this.path = path;
		this.watchForNewFiles = watchForNewFiles;
		this.controller = controller;
	}
	
	/**
	 * Watches a directory for changes and if changes are detected
	 * it checks if the changed file is in our file list 
	 * or if its a recursively watched directory it monitors new files as well
	 * 
	 * @param path
	 */
	public void run() {
		try {
				WatchService watcher = FileSystems.getDefault().newWatchService();

				// Register
				Path dir = Paths.get(path);
				dir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY);

				while (true) {

				    
				    try {
						
						
					    WatchKey key;
					    try {
					        // wait for a key to be available
					        key = watcher.take();
					    } catch (InterruptedException ex) {
					        return;
					    }
						
						 for (WatchEvent<?> event : key.pollEvents()) {
	
						        // get event type
						        WatchEvent.Kind<?> eventType = event.kind();

						        // get file name
						        @SuppressWarnings("unchecked")
						        WatchEvent<Path> ev = (WatchEvent<Path>) event;
						        Path fileName = ev.context();
						        String fullPath = path+fileName.toString();
						        FileInfo newFileInfo = null;
						        
						        
						        File newFile = new File(fullPath);
						        
					        	if (newFile.exists() && !newFile.isDirectory()) {
							        newFileInfo = new FileInfo(fullPath);
					        	} 
						        
						        System.out.println("FileWatcher: "+path);
						        System.out.println(eventType.name() + ": " + fullPath);
						        						        
						        // Check if file is in list
						        FileWatcherController.semaphore.acquire();
						        
						        FileInfo fileFromList = null;
						        
						        // New file was created -> Add to list
						        if (watchForNewFiles && eventType == ENTRY_CREATE) {						        	
						        	
						        	if (newFile.isDirectory()) {
						        		controller.watchDirectoryRecursively(fullPath+"/");
						        	} else {
						        		
								        newFileInfo = new FileInfo(fullPath);
						        		fileFromList = controller.getWatchedFileFromListByHash(newFileInfo.checksum);
	
						        		// Add file to watchList if its not already inside
							        	if (fileFromList == null) {
							        		controller.watchFile(fullPath, false);
							        	}
						        	}
						        } else {
						        	fileFromList = controller.getWatchedFileFromListByFileName(fullPath);
						        	
							        if (fileFromList != null) { // If file is in list
							        	
								        if (eventType == OVERFLOW || eventType == ENTRY_DELETE) {
				
								        	// DEL
								        	controller.deleteFileFromLists(fileFromList);
								        	
								        } else if (eventType == ENTRY_MODIFY) {

								            // DEL
								        	controller.deleteFileFromLists(fileFromList);
								        	
								        	// ADD
								        	controller.addFileToLists(fullPath);

								        }
								        
							        } else {
						        		System.out.println("Don't monitor for changes cause file not in list: "+fullPath);
							        }
						        }
						        
						        FileWatcherController.semaphore.release();
						        
						    }						
						 
						    // Reset key
						    boolean valid = key.reset();
						    if (!valid) {
						        break;
						    }
											
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
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
