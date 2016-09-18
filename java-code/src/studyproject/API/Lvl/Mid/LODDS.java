package studyproject.API.Lvl.Mid;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Vector;

import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileHasher;
import studyproject.API.Lvl.Mid.Core.ConnectionInfo;
import studyproject.API.Lvl.Mid.Core.FileChange;
import studyproject.API.Lvl.Mid.Core.RemoteFileInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class LODDS {
	
	private long lastChange;
	private Vector<FileChange> localFileChanges;
	private Vector<UserInfo> clientList;
	private Vector<RemoteFileInfo> availableFiles;
	private Vector<String> sharedFolders; 
	private long load;
	private String interfaceName;
	private int advertisePort;
	private int listenPort;
	@SuppressWarnings("unused")
	private String userName;

	public void startAdvertising(){
	}
	
	public void stopAdvertising(){
	}
	
	public void startListening(){
	}
	
	public void stopListening(){
	}
	
	public void getFile(String user, String checksum, String localPath, int startIndex, int endIndex){
		FileConnectionThread fileConnectionThread = new FileConnectionThread(getUserConnectionInfo(user),
				checksum, getFileSize(checksum), localPath, startIndex, endIndex);
		fileConnectionThread.run();
	}
	
	public void getFile(String user, String checksum, String localPath){
		FileConnectionThread fileConnectionThread = new FileConnectionThread(getUserConnectionInfo(user),
				checksum, getFileSize(checksum), localPath);
		fileConnectionThread.run();
	}
	
	public UserInfo getUserConnectionInfo(String user){
		for(UserInfo currentUser: clientList){
			if(currentUser.getUserName().equals(user));
			return currentUser;
		}
		return null;
	}
	
	public int shareFolder(String path){
		if(Files.exists(Paths.get(path)) && Files.isDirectory(Paths.get(path)) && ! sharedFolders.contains(path)){
			sharedFolders.add(path);
			//TODO tell the fileWatcher to start tracking this dir
			try {
				Files.walk(Paths.get(path)).forEach(filePath -> {
					if (Files.isRegularFile(filePath)) {
						try {
							localFileChanges.add(new FileChange(path, Files.size(Paths.get(path)), FileHasher.getFileHash(path),
									FileAction.add, System.currentTimeMillis() / 1000));
						} catch (Exception e) {
							//TODO what to do here?
						}
					}
				});
				setLastChange(System.currentTimeMillis() / 1000);
				return 0;
			} catch (IOException e) {
				return 1;
			}
		}
		return 4;
	}
	
	public int unshareFolder(String path){
		if(sharedFolders.contains(path)){
			sharedFolders.remove(path);
			for(FileChange fileChange: localFileChanges){
				if(fileChange.getPath().startsWith(path)){
					localFileChanges.remove(fileChange);
				}
			}
			setLastChange(System.currentTimeMillis() /1000);
			//TODO tell fileWatcher to stop tracking this dir
			return 0;
		}
		return 4;
	}
	
	public Vector<String> getSharedFolders(){
		return sharedFolders;
	}
	
	/**
	 *  //TODO wtf did we think here?
	 * @return
	 */
	public Vector<ConnectionInfo> getOutstandingBytes(){
		return null;
	}
	
	public Vector<FileChange> getFileChanges(long timeStamp){
		Vector<FileChange> fileChanges = new Vector<FileChange>();
		for(FileChange fileChange: localFileChanges){
			if(fileChange.getTimeStamp() > timeStamp){
				fileChanges.add(fileChange);
			}
		}
		return fileChanges;
	}
	
	public Vector<UserInfo> getUsers(){
		return clientList;
	}
	
	public void setInterface(String interfaceName){
		this.interfaceName = interfaceName;
	}
	
	public String getInterface(){
		return interfaceName;
	}
	
	public void setAdvertisePort(int port){
		this.advertisePort = port;
	}
	
	public int getAdvertisePort(){
		return advertisePort;
	}
	
	public void setListenPort(int port){
		this.listenPort = port;
	}
	
	public int getListenPort(){
		return listenPort;
	}
	
	public void setUserName(String userName){
		this.userName = userName;
	}

	/**
	 * @return the lastChange
	 */
	public long getLastChange() {
		return lastChange;
	}

	/**
	 * @param lastChange the lastChange to set
	 */
	public void setLastChange(long lastChange) {
		this.lastChange = lastChange;
	}

	/**
	 * @return the load
	 */
	public long getLoad() {
		return load;
	}

	/**
	 * @param load the load to set
	 */
	public void setLoad(long load) {
		this.load = load;
	}
	
	private long getFileSize(String checksum){
		for(RemoteFileInfo fileInfo: availableFiles){
			if(fileInfo.getChecksum().equals(checksum)){
				return fileInfo.getSize();
			}
		}
		return 0;
	}
	
}
