package studyproject.API.Lvl.Mid;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;

import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileHasher;
import studyproject.API.Lvl.Low.Broadcast;
import studyproject.API.Lvl.Mid.Core.ConnectionInfo;
import studyproject.API.Lvl.Mid.Core.FileChange;
import studyproject.API.Lvl.Mid.Core.RemoteFileInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class LODDS {
	
	private final int defaultIpPort = 9002;
	private final int defaultAdvertisePort = 9002;
	
	private long lastChange;
	private Vector<FileChange> localFileChanges;
	private Vector<UserInfo> clientList;
	private ConcurrentHashMap<String, RemoteFileInfo> availableFiles;
	private Vector<String> sharedFolders; 
	private long load;
	private String interfaceName;
	private int advertisePort;
	private int listenPort;
	private String userName;
	private BroadcastSenderThread broadcastSender;
	private BroadcastListenerThread broadcastListener;
	private String broadcastAddress;
	private String networkAddress;
	private int ipPort;
	private int timeInterval = 1000;
	
	public LODDS(String interfaceName, String userName){
		localFileChanges = new Vector<FileChange>();
		clientList = new Vector<UserInfo>();
		availableFiles = new ConcurrentHashMap<String, RemoteFileInfo>();
		sharedFolders = new Vector<String>();
		ipPort = defaultIpPort;
		advertisePort = defaultAdvertisePort;
		this.interfaceName = interfaceName;
		setNetworkAddresses();
		this.userName = userName;
		load = 0;
	}

	public void startAdvertising(){
		if(broadcastAddress == null || networkAddress == null){
			setNetworkAddresses();
		}
		broadcastSender = new BroadcastSenderThread(this);
		broadcastSender.start();
	}
	
	public void stopAdvertising(){
		broadcastSender.interrupt();
		broadcastSender = null;
	}
	
	public void startListening(){
		broadcastListener = new BroadcastListenerThread(this);
		broadcastListener.start();
	}
	
	public void stopListening(){
		broadcastListener.setStopThread(true);
	}
	
	public void getFile(String user, String checksum, String localPath, long startIndex, long endIndex){
		FileConnectionThread fileConnectionThread = new FileConnectionThread(getUserConnectionInfo(user),
				checksum, getFileSize(checksum), localPath, startIndex, endIndex);
		fileConnectionThread.start();
	}
	
	public void getFile(String user, String checksum, String localPath){
		FileConnectionThread fileConnectionThread = new FileConnectionThread(getUserConnectionInfo(user),
				checksum, getFileSize(checksum), localPath);
		fileConnectionThread.start();
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
		setNetworkAddresses();
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
	
	public String getBroadcastAddress() {
		return broadcastAddress;
	}

	public String getUserName() {
		return userName;
	}

	public String getNetworkAddress() {
		return networkAddress;
	}

	public int getIpPort() {
		return ipPort;
	}

	public int getTimeInterval() {
		return timeInterval;
	}
	
	private long getFileSize(String checksum){
		if(availableFiles.containsKey(checksum)){
			return availableFiles.get(checksum).getSize();
		}
		return 0;
	}
	
	private void setNetworkAddresses(){
		StringBuilder broadcastAddr = new StringBuilder();
		Broadcast.getBroadcastAddress(interfaceName, broadcastAddr);
		broadcastAddress = broadcastAddr.toString();
		StringBuilder networkAddr = new StringBuilder();
		Broadcast.getLocalIp(interfaceName, networkAddr);
		networkAddress = networkAddr.toString();
	}

}
