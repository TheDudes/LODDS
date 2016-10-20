package studyproject.API.Lvl.Mid;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;

import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileHasher;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Lvl.Low.Broadcast;
import studyproject.API.Lvl.Mid.Core.ConnectionInfo;
import studyproject.API.Lvl.Mid.Core.FileChange;
import studyproject.API.Lvl.Mid.Core.RemoteFileInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;

/**
 * Main class of the Mid-level API implementation of the LODDS protocol Default
 * IP port: 9002 Default advertise port: 9002 Default interval between
 * broadcasts: 1000ms
 * 
 * @author Michael
 *
 */
public class LODDS {

	private final int DEFAULT_IP_PORT = 9002;
	private final int DEFAULT_ADVERTISE_PORT = 9002;
	private final int DEFAULT_TIME_INTERVAL = 1000;

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
	private int timeInterval;

	// TODO wait for Marius to provide this hash map
	// whereas string is the hash of the fileInfo
	private ConcurrentHashMap<String, FileInfo> localFiles;

	/**
	 * Initiates all lists and maps, retrieves the local and broadcast IP from
	 * the interface name and sets the IP and advertise ports to the default
	 * (9002) Also sets the interval between broadcasts to 1000
	 * 
	 * @param interfaceName
	 *            the name of the interface the client should use to communicate
	 *            with the network
	 * 
	 * @param userName
	 *            the name under which this client will appear to other clients
	 */
	public LODDS(String interfaceName, String userName) {
		localFileChanges = new Vector<FileChange>();
		clientList = new Vector<UserInfo>();
		availableFiles = new ConcurrentHashMap<String, RemoteFileInfo>();
		sharedFolders = new Vector<String>();
		localFiles = new ConcurrentHashMap<>();
		ipPort = DEFAULT_IP_PORT;
		advertisePort = DEFAULT_ADVERTISE_PORT;
		timeInterval = DEFAULT_TIME_INTERVAL;
		this.interfaceName = interfaceName;
		setNetworkAddresses();
		this.userName = userName;
		load = 0;
	}

	/**
	 * creates a BroadcastSenderThread and starts it. time between broadcasts is
	 * the value of LODDS.getTimeInterval this interval can be changed on the
	 * fly
	 */
	public void startAdvertising() {
		if (broadcastAddress == null || networkAddress == null) {
			setNetworkAddresses();
		}
		broadcastSender = new BroadcastSenderThread(this);
		broadcastSender.start();
	}

	/**
	 * sends an interrupt to the BroadcastSenderThread which prompts it to stop
	 * advertising
	 */
	public void stopAdvertising() {
		broadcastSender.interrupt();
		broadcastSender = null;
	}

	/**
	 * creates a new BroadcastListenerThread and starts it
	 */
	public void startListening() {
		broadcastListener = new BroadcastListenerThread(this);
		broadcastListener.start();
	}

	/**
	 * sets the stopThread variable in the BroadcastListener to true which
	 * prompts the Thread to stop listening
	 */
	public void stopListening() {
		broadcastListener.setStopThread(true);
	}

	/**
	 * starts a new fileSenderThread which will send the whole file specified by
	 * the checksum to the user specified by its username
	 * 
	 * @param user
	 *            the unique user name
	 * @param checksum
	 *            the checksum of the file to send
	 */
	public void sendFile(String user, String checksum) {
		FileSenderThread fileSenderThread = new FileSenderThread(getUserConnectionInfo(user), localFiles.get(checksum));
		fileSenderThread.start();
	}

	/**
	 * starts a new fileSenderThread which will send a part of the file
	 * specified by the checksum to the user specified by its username
	 * 
	 * @param user
	 *            the unique user name
	 * @param checksum
	 *            the checksum of the file to send
	 * @param startIndex
	 *            the index(number of byte) where the transaction shall start
	 * @param endIndex
	 *            the index(number of byte) where the transaction shall end
	 */
	public void sendFile(String user, String checksum, long startIndex, long endIndex) {
		FileSenderThread fileSenderThread = new FileSenderThread(getUserConnectionInfo(user), localFiles.get(checksum),
				startIndex, endIndex);
		fileSenderThread.start();
	}

	/**
	 * get a whole or parts of a file from another user Starts a
	 * FileConnectionThread and starts it, the thread does all the work
	 * 
	 * @param user
	 *            the user from which to get the file from
	 * 
	 * @param checksum
	 *            the checksum of the file
	 * 
	 * @param localPath
	 *            the complete path under which the file should be saved
	 * 
	 * @param startIndex
	 *            the index from which to begin the file transfer
	 * 
	 * @param endIndex
	 *            the index at which to stop the file transfer
	 */
	public void getFile(String user, String checksum, String localPath, long startIndex, long endIndex) {
		FileConnectionThread fileConnectionThread = new FileConnectionThread(getUserConnectionInfo(user), checksum,
				getFileSize(checksum), localPath, startIndex, endIndex, this);
		// TODO set outstanding bytes on outgoing connection?
		fileConnectionThread.start();
	}

	/**
	 * get a whole file from another user Uses the getFile() method of this
	 * class with start and endIndex
	 * 
	 * @param user
	 *            the user from which to get the file from
	 * 
	 * @param checksum
	 *            the checksum of the file
	 * 
	 * @param localPath
	 *            the complete path under which the file should be saved
	 * 
	 * @param startIndex
	 *            the index from which to begin the file transfer
	 * 
	 * @param endIndex
	 *            the index at which to stop the file transfer
	 */
	public void getFile(String user, String checksum, String localPath) {
		FileConnectionThread fileConnectionThread = new FileConnectionThread(getUserConnectionInfo(user), checksum,
				getFileSize(checksum), localPath, this);
		fileConnectionThread.start();
	}

	/**
	 * updates the list of shared files of a specified user
	 * 
	 * @param user
	 *            the user of which to update the list of shared files
	 */
	public void updateFileInfo(String user) {
		UpdateFileInfoThread updateFileInfoThread = new UpdateFileInfoThread(getUserConnectionInfo(user), this);
		updateFileInfoThread.start();
	}

	/**
	 * Sends a file with the specified checksum to the specified user if he
	 * accepted the request in the specified timeout. If not the function
	 * returns.
	 * 
	 * @param user
	 * @param checksum
	 */
	public void sendFileWP(String user, String checksum) {

	}

	/**
	 * Gets a file if a SendPermission was received
	 */
	public void getFileWP() {

	}

	/**
	 * get the information necessary to connect to another user, this means IP
	 * address and port
	 * 
	 * @param user
	 *            the name of the user
	 * 
	 * @return UserInfo that contains information about the IP and port of the
	 *         user
	 */
	public UserInfo getUserConnectionInfo(String user) {
		for (UserInfo currentUser : clientList) {
			if (currentUser.getUserName().equals(user))
				return currentUser;
		}
		return null;
	}

	/**
	 * Make a folder and its contents available to other clients
	 * 
	 * @param path
	 *            the path to the folder
	 * 
	 * @return 0 or error codes
	 */
	public int shareFolder(String path) {
		if (Files.exists(Paths.get(path)) && Files.isDirectory(Paths.get(path)) && !sharedFolders.contains(path)) {
			sharedFolders.add(path);
			// TODO tell the fileWatcher to start tracking this dir
			try {
				Files.walk(Paths.get(path)).forEach(filePath -> {
					if (Files.isRegularFile(filePath)) {
						try {
							localFileChanges.add(new FileChange(path, Files.size(Paths.get(path)),
									FileHasher.getFileHash(path), FileAction.add, System.currentTimeMillis() / 1000));
						} catch (Exception e) {
							// TODO what to do here?
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

	/**
	 * removes a folder and its contents from the list of files other clients
	 * can get from this client
	 * 
	 * @param path
	 *            the path of the folder
	 * 
	 * @return 0 or error codes
	 */
	public int unshareFolder(String path) {
		if (sharedFolders.contains(path)) {
			sharedFolders.remove(path);
			for (FileChange fileChange : localFileChanges) {
				if (fileChange.getPath().startsWith(path)) {
					localFileChanges.remove(fileChange);
				}
			}
			setLastChange(System.currentTimeMillis() / 1000);
			// TODO tell fileWatcher to stop tracking this dir
			return 0;
		}
		return 4;
	}

	/**
	 * get a list of all folders this client shares
	 * 
	 * @return list of all folders this client shares
	 */
	public Vector<String> getSharedFolders() {
		return sharedFolders;
	}

	/**
	 * //TODO wtf did we think here?
	 * 
	 * @return
	 */
	public Vector<ConnectionInfo> getOutstandingBytes() {
		return null;
	}

	public Vector<FileChange> getFileChanges(long timeStamp) {
		Vector<FileChange> fileChanges = new Vector<FileChange>();
		for (FileChange fileChange : localFileChanges) {
			if (fileChange.getTimeStamp() > timeStamp) {
				fileChanges.add(fileChange);
			}
		}
		return fileChanges;
	}

	/**
	 * 
	 * @return a list with all known other clients
	 */
	public Vector<UserInfo> getUsers() {
		return clientList;
	}

	/**
	 * 
	 * @param interfaceName
	 *            the interface that should be used to communicate with the
	 *            network
	 */
	public void setInterface(String interfaceName) {
		this.interfaceName = interfaceName;
		setNetworkAddresses();
	}

	/**
	 * 
	 * @return the name of the interface currently used to communicate with the
	 *         network
	 */
	public synchronized String getInterface() {
		return interfaceName;
	}

	/**
	 * 
	 * @param port
	 *            the port that broadcasts are sent to/retrieved from
	 */
	public synchronized void setAdvertisePort(int port) {
		this.advertisePort = port;
	}

	/**
	 * 
	 * @return the port used to broadcast to/retrieve broadcasts from
	 */
	public synchronized int getAdvertisePort() {
		return advertisePort;
	}

	/**
	 * 
	 * @param port
	 *            the port this client should listen to incoming IP connections
	 *            on
	 */
	public synchronized void setListenPort(int port) {
		this.listenPort = port;
	}

	/**
	 * 
	 * @return the port currently used by this client for incoming IP
	 *         connections
	 */
	public synchronized int getListenPort() {
		return listenPort;
	}

	/**
	 * 
	 * @param userName
	 *            the name this client should be displayed as on other clients
	 */
	public void setUserName(String userName) {
		this.userName = userName;
	}

	/**
	 * @return the time stamp of the last change in the file system of this
	 *         client
	 */
	public synchronized long getLastChange() {
		return lastChange;
	}

	/**
	 * @param lastChange
	 *            the time stamp of the last change in the file system of this
	 *            client
	 */
	public synchronized void setLastChange(long lastChange) {
		this.lastChange = lastChange;
	}

	/**
	 * @return the number of bytes that this client still has to send
	 */
	public synchronized long getLoad() {
		return load;
	}

	/**
	 * @param load
	 *            the number of bytes that this client still has to send
	 */
	@Deprecated
	public synchronized void setLoad(long load) {
		this.load = load;
	}

	/**
	 * @param load
	 *            the number of bytes that should be added to the current load
	 */
	public synchronized void incrementLoad(long load) {
		this.load += load;
	}

	/**
	 * @param load
	 *            the number of bytes that should be subtracted from the current
	 *            load
	 */
	public synchronized void decrementLoad(long load) {
		if (this.load >= load) {
			this.load = -load;
		}
	}

	/**
	 * 
	 * @return the IP address to which this client should send broadcasts to
	 */
	public synchronized String getBroadcastAddress() {
		return broadcastAddress;
	}

	/**
	 * 
	 * @return the name that this client is currently know as in the network
	 */
	public synchronized String getUserName() {
		return userName;
	}

	/**
	 * 
	 * @return the IP address of the interface used by this client to
	 *         communicate with the network
	 */
	public synchronized String getNetworkAddress() {
		return networkAddress;
	}

	/**
	 * 
	 * @return the port used to listen to incoming IP connections
	 */
	public synchronized int getIpPort() {
		return ipPort;
	}

	/**
	 * 
	 * @return the interval between broadcasts from this client
	 */
	public synchronized int getTimeInterval() {
		return timeInterval;
	}

	/**
	 * 
	 * @return the hashMap with all shared files in the network
	 */
	public ConcurrentHashMap<String, RemoteFileInfo> getAvailableFiles() {
		return availableFiles;
	}

	private long getFileSize(String checksum) {
		if (availableFiles.containsKey(checksum)) {
			return availableFiles.get(checksum).getSize();
		}
		return 0;
	}

	private void setNetworkAddresses() {
		StringBuilder broadcastAddr = new StringBuilder();
		Broadcast.getBroadcastAddress(interfaceName, broadcastAddr);
		broadcastAddress = broadcastAddr.toString();
		StringBuilder networkAddr = new StringBuilder();
		Broadcast.getLocalIp(interfaceName, networkAddr);
		networkAddress = networkAddr.toString();
	}

}
