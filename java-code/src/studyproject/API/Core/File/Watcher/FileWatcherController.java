package studyproject.API.Core.File.Watcher;

import java.io.File;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Iterator;
import java.util.Vector;

import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Core.File.InfoList.FileInfoListEntry;

/**
 * TODO:
 * - Handle file and folder removal
 * - Restructure
 */
public class FileWatcherController {
	
	// File List of files that are being watched
	public Vector<FileInfoListEntry> fileInfoList = new Vector<FileInfoListEntry>();
	
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

		myWatchService.watchDirectoryRecursively("testData/FileWatcherController/");
	}
	
	/**
	 *  'get info timestamp\n'
      Will request an update about his files from a client. Timestamp
      is specifying the last known state. So if client A changes his
      files the timestamp he broadcasts will update. If client B sees
      the change he will send 'get info timestamp' to A, specifying
      the last timestamp of A he saw. So A will respond with all his
      updates since the given timestamp. If timestamp is 0, a complete
      list of shared files from the specific client is requested.
	 * @param timestamp
	 */
	public String getInfo(long timestamp) {
		String header = "";
		String body = "";
		
		// Body
		int filesMatched = 0;
		for (FileInfoListEntry file:fileInfoList) {
			
			if (timestamp == 0 || (((file.file.lastModified()/ 1000L) >= timestamp) || (file.timestampAdded >= timestamp))) {
				/*
				System.out.println("query timestamp: "+timestamp);
				System.out.println("last modified: "+file.file.lastModified());
				System.out.println("timestamp added: "+file.timestampAdded);
				System.out.println();*/
				body = this.convertFileInfoToString(file)+body;
				filesMatched++;
			} 

		}
		
		// Header
		if (timestamp == 0) {
		   Long timestampNow =  System.currentTimeMillis() / 1000L;
		   header = "all "+timestampNow+" "+filesMatched+"\n";
		} else {
		   header = "upd "+timestamp+" "+filesMatched+"\n";
		}
		
		return header+body;
	}
	
	public String convertFileInfoToString(FileInfo file) {
		return file.fileAction.name()+" "+file.checksum+" "+file.size+" "+file.fileName+"\n";		
	}
	
	/**
	 * Adds new file to watcher
	 * @param path
	 * @param watchParentFolderForNewFiles
	 * @throws NoSuchAlgorithmException
	 * @throws IOException
	 */
	public void watchFile(String path, Boolean watchParentFolderForNewFiles) throws NoSuchAlgorithmException, IOException {
		// System.out.println("watchFile: "+path);
		// Create new FileInfo object and add it to vector list
		FileInfoListEntry newFile = addNewFile(path);
				
		// Add parent directory of file to watchedDirectories if its not already inside
		if (!watchedInternalDirectories.contains(newFile.parentDirectory)) {
			watchedInternalDirectories.add(newFile.parentDirectory);
			
			// Start to watch directory
			watchDirectory(newFile.parentDirectory, watchParentFolderForNewFiles);
		}
		
	}
	
	/**
	 * Starts directory listener thread
	 * @param path
	 */
	private void watchDirectory(String path, Boolean watchForNewFiles) {
        (new Thread(new FileWatcher(path, watchForNewFiles, this))).start();

	}
	
	/**
	 * Watches all files and folders from a directory recursively
	 * @param fileName
	 * @throws NoSuchAlgorithmException
	 * @throws IOException
	 */
	public void watchDirectoryRecursively(String fileName) throws NoSuchAlgorithmException, IOException {
		System.out.println("watchDirectoryRecursively: "+fileName);
		
		// Start to watch directory
		watchDirectory(fileName, true);
		
		File[] files = new File(fileName).listFiles();

		 if (files != null) {
			 for (File file : files) {
			        if (file.isDirectory()) {
			            	watchDirectoryRecursively(file.getPath().toString());
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
		
	    Iterator<FileInfoListEntry> itr = fileInfoList.iterator();
	    FileInfoListEntry currentElement = null;
	    
		while(itr.hasNext())
		  currentElement = itr.next();
	      if (currentElement.fileName.equals(fileName)) {
	    	  return currentElement;
	      }
	  
		return null;
	}
	
	public void addDeletedFile(FileInfo file) {
		System.out.println("Add del file: "+file.fileName);

		Long timestamp = System.currentTimeMillis() / 1000L;
    	FileInfoListEntry deletedFile = new FileInfoListEntry(file.checksum, file.size, file.fileName, FileAction.del, timestamp);
    	fileInfoList.add(deletedFile);
	}
	
	public FileInfoListEntry addNewFile(String fileName) throws NoSuchAlgorithmException, IOException {
		FileInfoListEntry newFile = new FileInfoListEntry(fileName);
		fileInfoList.add(newFile);
		System.out.println("Add new file: "+fileName);
		return newFile;
	}
	
}
