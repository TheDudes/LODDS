package studyproject.API.Core.File.Watcher;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Core.File.InfoList.FileInfoListEntry;

/**
 * TODO: - Bug: When a folder is deleted WatchService does not notify about the
 * deleted files that were inside that folder
 */
public class FileWatcherController {

	// Hash map that contains all files that are actively being watched, files
	// are grouped by hash in lists
	public ConcurrentHashMap<String, Vector<FileInfoListEntry>> currentFilesListHashListAsKey = new ConcurrentHashMap<String, Vector<FileInfoListEntry>>();

	// Hash map that contains all files that are actively being watched,
	// fileName as key
	// public ConcurrentHashMap<String, FileInfoListEntry>
	// currentFilesListFileNameAsKey = new ConcurrentHashMap<String,
	// FileInfoListEntry>();

	// Tree that contains all files that are being watched
	public FileWatcherTreeNode currentFiles = new FileWatcherTreeNode(true);

	// History of all file modifications as specified in the protocol
	public Vector<FileInfoListEntry> fileInfoHistory = new Vector<FileInfoListEntry>();

	// Javas watchService can only watch directories, no single files
	// In order to watch a file we need to watch the whole directory
	// and than filter for file updates
	public Vector<String> watchedInternalDirectories = new Vector<String>();

	// Helps to prevent that files are added multiple times
	// static Semaphore semaphore = new Semaphore(0);
	public ReentrantLock lock = new ReentrantLock();
	
	// Timestamp of first share
	private Long firstShareTimestamp;
	
	public ConcurrentHashMap<String, FileWatcher> fileWatcherThreads = new ConcurrentHashMap<>();

	/**
	 * Test code
	 * 
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		try {

			FileWatcherController c = new FileWatcherController();
			String virtualRoot = "/Users/robinhood/Desktop/testData/";

			c.watchDirectoryRecursively("/Users/robinhood/Desktop/testData/", virtualRoot);

			while (true) {
				System.out.println("");
				System.out.println(c.getInfo(0));
				System.out.println("Watched directories:");
				System.out.println(c.watchedInternalDirectories);
				Thread.sleep(5 * 1000);
				c.currentFiles.printTree();
				c.printThreadsInfo();
			}

		} catch (Exception e) {
			System.out.println("PRINT STACK TRACE");
			e.printStackTrace();
		}

	}

	public void unwatchDirectory(String directory) {
		watchedInternalDirectories.remove(directory);
	}
	
	@SuppressWarnings("deprecation")
	public void stopWatchThread(String relativePath) {
		System.out.println("Stop watch thread: " + relativePath);
		
		fileWatcherThreads.forEach((k,v) -> {
			if (v.isAlive() && k == relativePath) {
				System.out.println("Interrupting watch thread: " + relativePath);
				v.interrupt();
				try {
					v.key.cancel();
					v.watcher.close();
					
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				
				
				v.stop();
				
			} else {
				System.out.println("Already not alive anymore: " + relativePath);
			}
		});
	}

	/**
	 * 'get info timestamp\n' 
	 * 
	 * Returns file history since given timestamp-1sec
	 * 
	 * @param timestamp
	 *            unix timestamp in seconds
	 */
	public String getInfo(long timestamp) {

		String header = "";
		String body = "";

		Long currentTimestampSec = System.currentTimeMillis() / 1000L;
		Long currentTimestampMinusOneSec = currentTimestampSec - 1;

		Boolean shareAllFiles = false;
		
		if (this.firstShareTimestamp != null && timestamp < this.firstShareTimestamp) {
			shareAllFiles = true;
		}

		// Body
		int filesMatched = 0;
		for (FileInfoListEntry file : fileInfoHistory) {

			Long fileLastModifiedSec = file.file.lastModified() / 1000L;

			// Check if file was added after given timestamp but not during last sec
			Boolean addedCheck = file.timestampAdded >= timestamp
					&& file.timestampAdded < currentTimestampMinusOneSec;

			// Check if file was last modified after given timestamp but not during last sec
			Boolean lastModifiedCheck = fileLastModifiedSec >= timestamp
					&& fileLastModifiedSec < currentTimestampMinusOneSec;

			if (timestamp == 0 || lastModifiedCheck || addedCheck || shareAllFiles) {
				body = this.convertFileInfoToString(file) + body;
				filesMatched++;
			}

		}

		// Header
		if (timestamp == 0 || shareAllFiles) {
			header = "all " + currentTimestampSec + " " + filesMatched + "\n";
		} else {
			header = "upd " + currentTimestampSec + " " + filesMatched + "\n";
		}

		return header + body;
	}

	public String convertFileInfoToString(FileInfo file) {
		return file.fileAction.name() + " " + file.checksum + " " + file.size + " " + file.relativeFileName + "\n";
	}

	/**
	 * Adds new file to watcher
	 * 
	 * @param path
	 * @param watchParentFolderForNewFiles
	 * @throws Exception
	 */
	public void watchFile(String path, Boolean watchParentFolderForNewFiles, String virtualRoot) throws Exception {
		// System.out.println("watchFile: "+path);

		// Create new FileInfo object and add it to vector list
		FileInfoListEntry newFile = addFileToLists(path, virtualRoot);

		// Add parent directory of file to watchedDirectories if its not already
		// inside
		String parentDirectory = newFile.parentDirectory + "/";

		if (!watchedInternalDirectories.contains(parentDirectory)) {
			watchedInternalDirectories.add(parentDirectory);

			// Start to watch directory
			watchDirectory(parentDirectory, watchParentFolderForNewFiles, virtualRoot);
		}

	}

	/**
	 * Starts directory listener thread
	 * 
	 * @param path
	 */
	public void watchDirectory(String path, Boolean watchForNewFiles, String virtualRoot) {
		FileWatcher t = new FileWatcher(path, watchForNewFiles, this, virtualRoot);
		t.start();
		fileWatcherThreads.put(path, t);
	}
	
	public void printThreadsInfo() {
		fileWatcherThreads.forEach((k,v) -> {
			if (v.isAlive()) {
				System.out.println("Thread is alive: " + k);
			} else {
				System.out.println("Thread is not alive: " + k);
			}
		});
	}

	/**
	 * Watches all files and folders from a directory recursively. Virtual root
	 * directory will automatically be set to absoluteFileName
	 * 
	 * @param absoluteFileName
	 *            absolute file name in file system
	 * @throws Exception
	 */
	public void watchDirectoryRecursively(String absoluteFileName) throws Exception {
		watchDirectoryRecursively(absoluteFileName, absoluteFileName);
	}
	
	private Boolean isBeingWatched(String path) {
		if (watchedInternalDirectories.contains(path)) 
			return true;
		
		for (String dir:watchedInternalDirectories) {
		    Path watched = Paths.get(dir).toAbsolutePath();

			if (isChild(watched, path)) {
				return true;
			}
		}
		
		return false;
		
	}
	
	private Boolean isChild(Path child, String parentText) {
	    Path parent = Paths.get(parentText).toAbsolutePath();
	    return child.startsWith(parent);
	}

	/**
	 * Watches all files and folders from a directory recursively
	 * 
	 * @param absoluteFileName
	 *            absolute file name in file system
	 * @param virtualRoot
	 *            virtual root directory (part of absolute filename)
	 * @throws Exception
	 */
	public void watchDirectoryRecursively(String absoluteFileName, String virtualRoot) throws Exception {

		// Start to watch directory
		watchDirectory(absoluteFileName, true, virtualRoot);

		if (!isBeingWatched(absoluteFileName)) {
			System.out.println("New directory will be watched: " + absoluteFileName);
			watchedInternalDirectories.add(absoluteFileName);
		}

		File[] files = new File(absoluteFileName).listFiles();

		if (files != null) {
			for (File file : files) {
				if (file.isDirectory()) {
					watchDirectoryRecursively(file.getPath().toString() + File.separator, virtualRoot);
				} else {
					watchFile(file.getPath().toString(), true, virtualRoot);
				}
			}
		}
	}

	/**
	 * Returns null if there is no file with the given hash in the list Returns
	 * a FileInfo object if there is a file with the given hash in the list
	 * 
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
		return (FileInfo) currentFiles.getFileInfoListEntryByFileName(fileName);
	}
	
	/**
	 * Deletes folder including all subdirectories and files from list
	 * @param node
	 */
	public void deleteFolderFromLists(String fileName) {	
		
		System.out.println("Delete folder from lists: " + fileName);
		
		// Get all fileInfoList entries
		ArrayList<FileInfoListEntry> entries = FileWatcherTreeNode.removeFileNameAndGetRemovedFileInfoListEntries(currentFiles, fileName);
		
		if (entries.size() > 0) {
			System.out.println("Deleted folder seems to have children");
			// Call deleteFileFromLists for all fileInfoListEntries inside that dir but not the parent one cause it's a dir
			for (FileInfoListEntry entry:entries) {
					System.out.println("Recursive delete call: "+entry.fileName);
					deleteFileFromLists(entry);
			}
		}
	}

	public void deleteFileFromLists(FileInfo file) {
		System.out.println("\ndeleteFileFromLists(): "+file.fileName);

		String virtualRoot = "";
		Boolean foundInList = false;

		// Remove from currentFilesListHashListAsKey
		List<FileInfoListEntry> filesToDelete = new ArrayList<FileInfoListEntry>();
		if (currentFilesListHashListAsKey.containsKey(file.checksum)) {
			for (FileInfoListEntry listFile : currentFilesListHashListAsKey.get(file.checksum)) {
				if (listFile.fileName.equals(file.fileName)) {
					filesToDelete.add(listFile);
					virtualRoot = listFile.virtualRoot;
					foundInList = true;
				}
			}
		}

		for (FileInfoListEntry fileToDelete : filesToDelete) {
			currentFilesListHashListAsKey.get(file.checksum).remove(fileToDelete);
		}
		
		// Remove from currentFiles tree
		FileWatcherTreeNode.removeFileNameAndGetRemovedFileInfoListEntries(currentFiles, file.fileName);
		
		if (currentFilesListHashListAsKey.size() == 0) {
			this.firstShareTimestamp = null;
		}
		
		if (foundInList) {
			// Create DEL entry for FileInfoList:
			
			// Create FileInfoListEntry object
			Long timestamp = System.currentTimeMillis() / 1000L;
			System.out.println("Create deleted file for "+file.fileName+" VR: "+virtualRoot);
			FileInfoListEntry deletedFile = new FileInfoListEntry(file.checksum, file.size, file.fileName, FileAction.del,
					timestamp, virtualRoot);

			// Add to FileInfoList
			fileInfoHistory.add(deletedFile);	
		}

	}
	
	private void setFirstShareTimestamp() {
		this.firstShareTimestamp = System.currentTimeMillis() / 1000L;
	}

	public FileInfoListEntry addFileToLists(String fileName, String virtualRoot) throws Exception {
		// System.out.println("Add new file: "+fileName);
		if (this.firstShareTimestamp == null) {
			this.setFirstShareTimestamp();
		}
		
		FileInfoListEntry existingFile = currentFiles.getFileInfoListEntryByFileName(fileName);

		if (existingFile == null) {
			
			FileInfoListEntry newFile = new FileInfoListEntry(fileName, virtualRoot);

			// Add to tree
			currentFiles.addFileInfoEntry(newFile.fileName, newFile);

			// Add to file info list
			fileInfoHistory.add(newFile);

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
			
			return newFile;

		} else {
			return existingFile;
		}

	}

	/**
	 * Get a FileInfo object of the shared file searched by its name.
	 * 
	 * @param Name
	 *            of the wanted file as String
	 * @return FileInfo object of the shared file
	 */
	public FileInfo getFileByName(String fileName) {
		return (FileInfo) currentFiles.getFileInfoListEntryByFileName(fileName);
	}

	/**
	 * Get a FileInfo object of the shared file searched by its checksum.
	 * 
	 * @param Checksum
	 *            of the wanted file as String
	 * @return FileInfo object of the shared file
	 */
	public FileInfo getFileByChecksum(String fileChecksum) {
		return (FileInfo) currentFilesListHashListAsKey.get(fileChecksum).get(0);
	}

}
