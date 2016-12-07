package studyproject.API.Core.File.Watcher;

import java.io.File;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.ReentrantLock;

import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Core.File.InfoList.FileInfoListEntry;

/**
 * TODO:
 * - Bug: When a folder is deleted WatchService does not notify about the deleted files that were inside that folder 
 */
public class FileWatcherController {

	// Hash map that contains all files that are actively being watched, files are grouped by hash in lists
	public ConcurrentHashMap<String, Vector<FileInfoListEntry>> currentFilesListHashListAsKey = new ConcurrentHashMap<String, Vector<FileInfoListEntry>>();
	
	// Hash map that contains all files that are actively being watched, fileName as key
	public ConcurrentHashMap<String, FileInfoListEntry> currentFilesListFileNameAsKey = new ConcurrentHashMap<String, FileInfoListEntry>();
	
	// Tree map that contains all files that are actively being watched, fileName as key
	// public DefaultMutableTreeNode currentFilesTree = new DefaultMutableTreeNode(null);

	// History of all file modifications as specified in the protocol
	public Vector<FileInfoListEntry> fileInfoList = new Vector<FileInfoListEntry>();
	
	// Javas watchService can only watch directories, no single files
	// In order to watch a file we need to watch the whole directory
	// and than filter for file updates
	public Vector<String> watchedInternalDirectories = new Vector<String>();
	
	// Helps to prevent that files are added multiple times
	static Semaphore semaphore = new Semaphore(0);
	
	public ReentrantLock lock = new ReentrantLock();


	/**
	 * Test code
	 * @param args
	 * @throws NoSuchAlgorithmException
	 * @throws IOException
	 * @throws InterruptedException 
	 */
	public static void main(String[] args) throws NoSuchAlgorithmException, IOException, InterruptedException {
		System.out.println("Start");

		FileWatcherController myWatchService = new FileWatcherController();
		String virtualRoot = "/Users/robinhood/Desktop/testData/";

		myWatchService.watchDirectoryRecursively("/Users/robinhood/Desktop/testData/", virtualRoot);
		
	    try {
	        while (true) {
	    		System.out.println("");
	    		System.out.println(myWatchService.getInfo(0));
	    		System.out.println("Watched directories:");
	    		System.out.println(myWatchService.watchedInternalDirectories);
	            Thread.sleep(5 * 1000);
	        }
	    } catch (InterruptedException e) {
	        e.printStackTrace();
	    }

	}
	
	public void unwatchDirectory(String directory) {
		System.out.println("Unwatch: "+directory);
		watchedInternalDirectories.remove(directory);
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
	 * @param timestamp unix timestamp in seconds
	 */
	public String getInfo(long timestamp) {
		
		System.out.println("getInfo(): "+timestamp);
		
		
		String header = "";
		String body = "";
		
		Long currentTimestampSec = System.currentTimeMillis() / 1000L;
		Long currentTimestampMinusOneSec = currentTimestampSec-1;
		
		// Body
		int filesMatched = 0;
		for (FileInfoListEntry file:fileInfoList) {
			
			Long fileLastModifiedSec = file.file.lastModified()/ 1000L;
			
			System.out.println("fileLastModifiedSec: "+fileLastModifiedSec);

			
			if (timestamp == 0 || ((fileLastModifiedSec >= timestamp) || (file.timestampAdded >= timestamp) && (fileLastModifiedSec < currentTimestampMinusOneSec) || (file.timestampAdded < currentTimestampMinusOneSec))) {
				
				
				body = this.convertFileInfoToString(file)+body;
				filesMatched++;
			} 

		}
		
		// Header
		if (timestamp == 0) {
		   header = "all "+currentTimestampSec+" "+filesMatched+"\n";
		} else {
		   header = "upd "+currentTimestampSec+" "+filesMatched+"\n";
		}
		
		return header+body;
	}
	
	public String convertFileInfoToString(FileInfo file) {
		return file.fileAction.name()+" "+file.checksum+" "+file.size+" "+file.relativeFileName+"\n";		
	}
	
	/**
	 * Adds new file to watcher
	 * @param path
	 * @param watchParentFolderForNewFiles
	 * @throws NoSuchAlgorithmException
	 * @throws IOException
	 */
	public void watchFile(String path, Boolean watchParentFolderForNewFiles, String virtualRoot) throws NoSuchAlgorithmException, IOException {
		System.out.println("watchFile: "+path);
		
		// Create new FileInfo object and add it to vector list
		FileInfoListEntry newFile = addFileToLists(path, virtualRoot);
				
		// Add parent directory of file to watchedDirectories if its not already inside
		String parentDirectory = newFile.parentDirectory+"/";
		
		if (!watchedInternalDirectories.contains(parentDirectory)) {
			watchedInternalDirectories.add(parentDirectory);
			
			// Start to watch directory
			watchDirectory(parentDirectory, watchParentFolderForNewFiles, virtualRoot);
		}
		
	}
	
	/**
	 * Starts directory listener thread
	 * @param path
	 */
	public void watchDirectory(String path, Boolean watchForNewFiles, String virtualRoot) {
		System.out.println("watchDirectory(): "+path);
        (new Thread(new FileWatcher(path, watchForNewFiles, this, virtualRoot))).start();
	}
	
	/**
	 * Watches all files and folders from a directory recursively. Virtual root directory will automatically be set to absoluteFileName
	 * @param absoluteFileName: absolute file name in file system
	 * @throws NoSuchAlgorithmException
	 * @throws IOException
	 */
	public void watchDirectoryRecursively(String absoluteFileName) throws NoSuchAlgorithmException, IOException {
		watchDirectoryRecursively(absoluteFileName,absoluteFileName);
	}
	
	/**
	 * Watches all files and folders from a directory recursively
	 * @param absoluteFileName: absolute file name in file system
	 * @param virtualRoot: virtual root directory (part of absolute filename)
	 * @throws NoSuchAlgorithmException
	 * @throws IOException
	 */
	public void watchDirectoryRecursively(String absoluteFileName, String virtualRoot) throws NoSuchAlgorithmException, IOException {
		System.out.println("watchDirectoryRecursively: "+absoluteFileName);
		
		// Start to watch directory
		System.out.println("watchDirRec**: "+absoluteFileName);
		watchDirectory(absoluteFileName, true, virtualRoot);
		
		if (!watchedInternalDirectories.contains(absoluteFileName))
			watchedInternalDirectories.add(absoluteFileName);
		
		File[] files = new File(absoluteFileName).listFiles();

		 if (files != null) {
			 for (File file : files) {
			        if (file.isDirectory()) {
			            	watchDirectoryRecursively(file.getPath().toString(), virtualRoot);
					 } else {
						 	watchFile(file.getPath().toString(), true, virtualRoot);
					 }
			 }
		 }
	}
	
	
	/**
	 * Returns null if there is no file with the given hash in the list
	 * Returns a FileInfo object if there is a file with the given hash in the list
	 * @param fileName
	 * @return FileInfo
	 */
	public FileInfo getWatchedFileFromListByHash(String hash) {
		if (!currentFilesListHashListAsKey.containsKey(hash) || currentFilesListHashListAsKey.get(hash).isEmpty()) {
			return null;
		} else {
			return currentFilesListHashListAsKey.get(hash).get(0);
		}
	}
	
	public FileInfo getWatchedFileFromListByFileName(String fileName) {
    	return currentFilesListFileNameAsKey.get(fileName);
	}
	
	public void deleteFileFromLists(FileInfo file) {
		System.out.println("deleteFileFromLists(): "+file.fileName);
		
		String virtualRoot = "";
		
    	// Remove from currentFilesListHashListAsKey
    	List<FileInfoListEntry> filesToDelete = new ArrayList<FileInfoListEntry>();
    	if (currentFilesListHashListAsKey.containsKey(file.checksum)) {
    		for (FileInfoListEntry listFile:currentFilesListHashListAsKey.get(file.checksum)) {
    			if (listFile.fileName.equals(file.fileName)) {
    				filesToDelete.add(listFile);
    				virtualRoot = listFile.virtualRoot;
    			}
    		}
    	}
    	
    	for (FileInfoListEntry fileToDelete:filesToDelete) {
    		currentFilesListHashListAsKey.get(file.checksum).remove(fileToDelete);
    	}
    	
    	// Remove from currentFilesListFileNameAsKey
    	currentFilesListFileNameAsKey.remove(file.fileName);

		// Create FileInfoListEntry object
		Long timestamp = System.currentTimeMillis() / 1000L;
    	FileInfoListEntry deletedFile = new FileInfoListEntry(file.checksum, file.size, file.fileName, FileAction.del, timestamp, virtualRoot);
    	
    	// Remove from fileInfoList
    	fileInfoList.add(deletedFile);

	}
	
	public FileInfoListEntry addFileToLists(String fileName, String virtualRoot) throws NoSuchAlgorithmException, IOException {
		System.out.println("Add new file: "+fileName);
		
		FileInfoListEntry newFile = new FileInfoListEntry(fileName, virtualRoot);
		Boolean fileShouldBeAdded = true;
		
		// Check if file is already inside the list with same hash to prevent multiple adds
		if (currentFilesListFileNameAsKey.containsKey(fileName)) {
			if (currentFilesListHashListAsKey.containsKey(newFile.checksum)) {
				if (currentFilesListHashListAsKey.get(newFile.checksum).get(0).fileName == newFile.fileName) {
					fileShouldBeAdded = false;
				}
			} 
		}
		
		if (fileShouldBeAdded) {
			
			System.out.println("File will be added");
			// Add to currentFilesListFileNameAsKey
			currentFilesListFileNameAsKey.put(newFile.fileName, newFile);

			// Add to file info list
			fileInfoList.add(newFile);
			
			// Add to currentFilesListHashListAsKey
			if (currentFilesListHashListAsKey.containsKey(newFile.checksum)) {
				// Add to existing list if hash group exists
				currentFilesListHashListAsKey.get(newFile.checksum).add(newFile);
			} else {
				// Create new list and add it if hash group does not exist
				Vector<FileInfoListEntry> fileList = new Vector<FileInfoListEntry>();
				fileList.add(newFile);
				currentFilesListHashListAsKey.put(newFile.checksum, fileList);
			}
			
			// Add to tree map
			// addNewFileToTree(newFile);

		}

		return newFile;
	}
	
	/**
	 * Get a FileInfo object of the shared file searched by its name.
	 * @param Name of the wanted file as String
	 * @return FileInfo object of the shared file
	 */
	public FileInfo getFileByName(String fileName) {
		return (FileInfo) currentFilesListFileNameAsKey.get(fileName);
	}

	/**
	 * Get a FileInfo object of the shared file searched by its checksum.
	 * @param Checksum of the wanted file as String
	 * @return FileInfo object of the shared file
	 */
	public FileInfo getFileByChecksum(String fileChecksum) {
		return (FileInfo) currentFilesListHashListAsKey.get(fileChecksum).get(0);
	}
	
	
}
