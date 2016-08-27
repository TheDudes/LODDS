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
import java.util.Iterator;
import java.util.Vector;

import studyproject.API.Core.File.FileInfo;

/**
 * TODO:
 * - Handle file and folder removal
 * - Restructure
 */
public class FileWatcherController {
	
	// File List of files that are being watched
	public Vector<FileInfo> fileInfoList = new Vector<FileInfo>();
	
	// Javas watchService can only watch directories, no single files
	// In order to watch a file we need to watch the whole directory
	// and than filter for file updates
	private Vector<String> watchedInternalDirectories = new Vector<String>();

	// Directories that should be watched recursively
	// private Vector<String> watchedRecursiveDirectories = new Vector<String>();
	
	/**
	 * Test code
	 * @param args
	 * @throws NoSuchAlgorithmException
	 * @throws IOException
	 */
	public static void main(String[] args) throws NoSuchAlgorithmException, IOException {
		System.out.println("Start");

		FileWatcherController myWatchService = new FileWatcherController();

		myWatchService.watchDirectoryRecursively("/Users/robinhood/Desktop/testDirectory");
	}
	
	/**
	 * Adds new file to watcher
	 * @param path
	 * @param watchParentFolderForNewFiles
	 * @throws NoSuchAlgorithmException
	 * @throws IOException
	 */
	public void watchFile(String path, Boolean watchParentFolderForNewFiles) throws NoSuchAlgorithmException, IOException {
		
		System.out.println("watchFile: "+path);
		
		// Create new FileInfo object and add it to vector list
		FileInfo newFile = new FileInfo(path);
		fileInfoList.addElement(newFile);
		
		// Add parent directory of file to watchedDirectories if its not already inside
		if (!watchedInternalDirectories.contains(newFile.parentDirectory)) {
			watchedInternalDirectories.add(newFile.parentDirectory);
			
			// Start to watch directory
	        (new Thread(new FileWatcher(newFile.parentDirectory, watchParentFolderForNewFiles, this))).start();
		}
		
	}
	
	/**
	 * Watches all files and folders from a directory recursively
	 * @param fileName
	 * @throws NoSuchAlgorithmException
	 * @throws IOException
	 */
	public void watchDirectoryRecursively(String fileName) throws NoSuchAlgorithmException, IOException {
		System.out.println("watchDirectoryRecursively: "+fileName);
		
		 File[] files = new File(fileName).listFiles();

		 if (files != null) {
			 for (File file : files) {
			        if (file.isDirectory()) {
			            for (File myFile:file.listFiles()) {
			            	watchDirectoryRecursively(myFile.getPath().toString());
			            }
					 } else {
						 	watchFile(file.getPath().toString(), true);
					 }
			 }
		 }
	}
	
	
	
	/**
	 * Returns null if there is no file with the given fileName in the list
	 * Returns a FileInfo object if there is a file with the given fileName in the list
	 * @param fileName
	 * @return FileInfo
	 */
	public FileInfo getWatchedFileFromList(String fileName) {
		
	    Iterator<FileInfo> itr = fileInfoList.iterator();
	    FileInfo currentElement = null;
	    
		while(itr.hasNext())
		  currentElement = itr.next();
	      if (currentElement.fileName.equals(fileName)) {
	    	  return currentElement;
	      }
	  
		return null;
	}
	
	public void addDeletedFile(FileInfo file) {
    	FileInfo deletedFile = new FileInfo(file.checksum, file.size, file.fileName, file.fileAction);
    	fileInfoList.add(deletedFile);
	}
	
	public void addNewFile(String fileName) throws NoSuchAlgorithmException, IOException {
		FileInfo newFile = new FileInfo(fileName);
		fileInfoList.add(newFile);
	}
	
}
