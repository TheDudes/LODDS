package studyproject.API.Lvl.Mid.Lodds;

import java.io.IOException;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.NoSuchAlgorithmException;
import java.util.Vector;

import javafx.collections.ObservableList;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Core.File.Watcher.FileWatcherController;
import studyproject.API.Loadbalancer.Loadbalancer;
import studyproject.API.Lvl.Low.Broadcast;
import studyproject.API.Lvl.Mid.BroadcastListenerThread;
import studyproject.API.Lvl.Mid.BroadcastSenderThread;
import studyproject.API.Lvl.Mid.FileConnectionThread;
import studyproject.API.Lvl.Mid.FileSenderThread;
import studyproject.API.Lvl.Mid.GetFileWPThread;
import studyproject.API.Lvl.Mid.SendFileWPThread;
import studyproject.API.Lvl.Mid.UpdateFileInfoThread;
import studyproject.API.Lvl.Mid.Core.ConnectionInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.API.Lvl.Mid.ThreadMonitoring.ThreadExecutor;

/**
 * Main class of the Mid-level API implementation of the LODDS protocol Default
 * IP port: 9002 Default advertise port: 9002 Default interval between
 * broadcasts: 1000ms
 * 
 * @author Michael
 *
 */
public class Lodds {

	private final int DEFAULT_IP_PORT = 9002;
	private final int DEFAULT_ADVERTISE_PORT = 9002;
	private final int DEFAULT_TIME_INTERVAL = 1000;
	private final int DEFAULT_PARALLEL_DOWNLOADS = 8;

	private long loadBalancingMinumum = 4096;

	private long lastChange;
	private Loadbalancer loadbalancer;
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
	private int parallelDownloads;
	private FileWatcherController watchService;
	private ThreadExecutor threadExecutor;
	private LoddsModel loddsModel;

	/**
	 * Initiates all lists and maps, sets the IP and advertise ports to the
	 * default (9002) Also sets the interval between broadcasts to 1000
	 * <p>
	 * <b>Call lodds.startUp() after you used this constructor and set the user
	 * name and interface name</b>
	 */
	public Lodds() {
		loddsModel = new LoddsModel();
		loddsModel.init();
		sharedFolders = new Vector<String>();
		ipPort = DEFAULT_IP_PORT;
		advertisePort = DEFAULT_ADVERTISE_PORT;
		timeInterval = DEFAULT_TIME_INTERVAL;
		parallelDownloads = DEFAULT_PARALLEL_DOWNLOADS;
		watchService = new FileWatcherController();
		this.interfaceName = "no interface selected";
		threadExecutor = new ThreadExecutor();
		this.userName = "UserNameNotSet!";
		load = 0;
		loadbalancer = new Loadbalancer(loadBalancingMinumum, parallelDownloads, threadExecutor);
	}

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
	public Lodds(String interfaceName, String userName) {
		sharedFolders = new Vector<String>();
		ipPort = DEFAULT_IP_PORT;
		advertisePort = DEFAULT_ADVERTISE_PORT;
		timeInterval = DEFAULT_TIME_INTERVAL;
		parallelDownloads = DEFAULT_PARALLEL_DOWNLOADS;
		watchService = new FileWatcherController();
		this.interfaceName = interfaceName;
		threadExecutor = new ThreadExecutor();
		setNetworkAddresses();
		this.userName = userName;
		load = 0;
		loadbalancer = new Loadbalancer(loadBalancingMinumum, parallelDownloads, threadExecutor);
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
		threadExecutor.execute(broadcastSender);
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
		threadExecutor.execute(broadcastListener);
	}

	/**
	 * sets the stopThread variable in the BroadcastListener to true which
	 * prompts the Thread to stop listening
	 */
	public void stopListening() {
		broadcastListener.setRun(false);
	}

	/**
	 * starts a new fileSenderThread which will send the whole file specified by
	 * the checksum to the user specified by its username
	 * 
	 * @param socket
	 *            the socket opened by the receiver
	 * @param fileInfo
	 *            the FileInfo which specifies the file which shall be sent
	 */
	public void sendFile(Socket socket, FileInfo fileInfo) {
		FileSenderThread fileSenderThread = new FileSenderThread(socket, fileInfo);
		threadExecutor.execute(fileSenderThread);
	}

	/**
	 * starts a new fileSenderThread which will send a part of the file
	 * specified by the checksum to the user specified by its username
	 * 
	 * @param socket
	 *            the socket opened by the receiver
	 * @param fileInfo
	 *            the FileInfo which specifies the file which shall be sent
	 * @param startIndex
	 *            the index(number of byte) where the transaction shall start
	 * @param endIndex
	 *            the index(number of byte) where the transaction shall end
	 */
	public void sendFile(Socket socket, FileInfo fileInfo, long startIndex, long endIndex) {
		FileSenderThread fileSenderThread = new FileSenderThread(socket, fileInfo, startIndex, endIndex);
		threadExecutor.execute(fileSenderThread);
	}

	/**
	 * get a whole or parts of a file from another user Starts a
	 * FileConnectionThread and starts it, the thread does all the work
	 * 
	 * @param user
	 *            the username of the user from which to get the file from
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
				getFileSize(checksum), localPath, startIndex, endIndex);
		threadExecutor.execute(fileConnectionThread);
	}

	/**
	 * get a whole file from another user Uses the getFile() method of this
	 * class with start and endIndex
	 * 
	 * @param user
	 *            the username of the user from which to get the file from
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
				getFileSize(checksum), localPath);
		threadExecutor.execute(fileConnectionThread);
	}

	/**
	 * if there is more than one user that shares the file the load is split
	 * between the clients with the lowest load. The parts of the file are
	 * stored as temporary files and then assembled into the real file after the
	 * download of the next part has finished
	 * 
	 * @param checksum
	 *            the checksum of the file to be pulled
	 * @param localPath
	 *            the absolute path to the location the file should be stored in
	 */
	public void getFileWithLoadBal(String checksum, String localPath) {
		Vector<UserInfo> owningUsers = getOwningUsers(checksum);
		if (owningUsers != null && owningUsers.size() == 0 || owningUsers == null) {
			// TODO error handling
		} else {
			if (owningUsers.size() == 1) {
				getFile(owningUsers.get(0).getUserName(), checksum, localPath);
			} else {
				if (getFileSize(checksum) < loadBalancingMinumum) {
					getFile(loadbalancer.getClientMinLoad(owningUsers, checksum), checksum, localPath);
				} else {
					loadbalancer.splitLoad(getOwningUsers(checksum), checksum, localPath, getFileSize(checksum));
				}
			}
		}
	}

	/**
	 * updates the list of shared files of a specified user
	 * 
	 * @param user
	 *            the user of which to update the list of shared files
	 */
	public void updateFileInfo(String user) {
		UpdateFileInfoThread updateFileInfoThread = new UpdateFileInfoThread(getUserConnectionInfo(user));
		threadExecutor.execute(updateFileInfoThread);
	}

	/**
	 * Sends a file with the specified checksum to the specified user if he
	 * accepted the request in the specified timeout. If not the function
	 * returns.
	 * 
	 * @param user
	 * @param checksum
	 */
	public void sendFileWP(String user, long timeout, FileInfo fileInfo) {
		SendFileWPThread sendFileWPThread = new SendFileWPThread(getUserConnectionInfo(user), timeout, fileInfo);
		threadExecutor.execute(sendFileWPThread);
	}

	/**
	 * Gets a file if a SendPermission was received
	 */
	public void getFileWP(Socket socket, String pathToSaveTo, String fileName, long fileSize) {
		GetFileWPThread getFileWPThread = new GetFileWPThread(socket, pathToSaveTo, fileName, fileSize);
		threadExecutor.execute(getFileWPThread);
	}

	// /**
	// * Respond a infolist about the watched files and directories
	// */
	// public void sendInfo() {
	// InfoSenderThread infoSenderThread = new InfoSenderThread();
	// infoSenderThread.run();
	// }

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
		for (UserInfo currentUser : loddsModel.getClientList()) {
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
			try {
				watchService.watchDirectoryRecursively(path);
			} catch (NoSuchAlgorithmException e) {
				// TODO Error Handling
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
				return 4;
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
			setLastChange(System.currentTimeMillis() / 1000);
			watchService.unwatchDirectory(path);
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

	// TODO implement getFileChanges(long timestamp)

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

	public ObservableList<UserInfo> getClientList() {
		return loddsModel.getClientList();
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

	// TODO create a getAllAvailableFiles method without having to construct a
	// list/hash map

	/**
	 * 
	 * @param checksum
	 *            the checksum of the file you want the size of
	 * @return the size of the file for which the checksum is given
	 */
	public long getFileSize(String checksum) {
		for (UserInfo userInfo : loddsModel.getClientList()) {
			if (userInfo.getChecksumToPath() != null && userInfo.getChecksumToPath().containsKey(checksum)) {
				return userInfo.getPathToFileInfo().get(userInfo.getChecksumToPath().get(checksum).firstElement())
						.getFilesize();
			}
		}
		return 0;
	}

	/**
	 * 
	 * @param checksum
	 *            the checksum of the file whose owners you want to know
	 * @return a vector of UserInfos with all users who have a file with the
	 *         given checksum
	 */
	public Vector<UserInfo> getOwningUsers(String checksum) {
		Vector<UserInfo> owningUsers = new Vector<UserInfo>();
		for (UserInfo userInfo : loddsModel.getClientList()) {
			if (userInfo.getChecksumToPath() != null && userInfo.getChecksumToPath().containsKey(checksum)) {
				owningUsers.add(userInfo);
			}
		}
		return owningUsers;
	}

	/**
	 * 
	 * @return the number of clients the Loadbalancer can pull files from at
	 *         once
	 */
	public int getParallelDownloads() {
		return parallelDownloads;
	}

	/**
	 * 
	 * @param parallelDownloads
	 *            the number of clients the Loadbalancer can pull files from at
	 *            once
	 */
	public void setParallelDownloads(int parallelDownloads) {
		this.parallelDownloads = parallelDownloads;
		loadbalancer.setParallelDownloads(parallelDownloads);
	}

	public LoddsModel getLoddsModel() {
		return loddsModel;
	}

	public void setLoddsModel(LoddsModel loddsModel) {
		this.loddsModel = loddsModel;
	}

	private void setNetworkAddresses() {
		StringBuilder broadcastAddr = new StringBuilder();
		Broadcast.getBroadcastAddress(interfaceName, broadcastAddr);
		broadcastAddress = broadcastAddr.toString();
		StringBuilder networkAddr = new StringBuilder();
		Broadcast.getLocalIp(interfaceName, networkAddr);
		networkAddress = networkAddr.toString();
	}

	public void startUp() {
		setNetworkAddresses();
		startAdvertising();
		startListening();
	}

	public FileWatcherController getWatchService() {
		return watchService;
	}

}
