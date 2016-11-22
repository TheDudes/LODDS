package studyproject.API.Loadbalancer;

import java.util.Collections;
import java.util.Vector;
import java.util.concurrent.Executor;

import studyproject.API.Lvl.Mid.FileConnectionThread;
import studyproject.API.Lvl.Mid.Core.UserInfo;

/**
 * The workhorse of the Loadbalancing module,
 * 
 * @author Michael
 *
 */
public class LoadbalancerMainThread extends Thread {

	private final long SLEEP_TIME = 500;

	private FileAssembler fileAssembler;
	private Vector<ProgressInfo> chunkThreads;
	LoadComparator loadComparator;
	Vector<UserInfo> owningUsers;
	private String checksum;
	private String localPath;
	private String tmpDirectory;
	private String fileName;
	private long chunksize;
	private long fileSize;
	private long lastStartIndex;
	private long transmittedData;
	private int chunknumber;
	private int parallelDownloads;
	private Executor executor;

	/**
	 * 
	 * @param checksum
	 *            the checksum of the file to pull
	 * @param localPath
	 *            the complete path of the file on the local directory, aka
	 *            where to put the file
	 * @param fileSize
	 *            the size of the file
	 * @param chunksize
	 *            the size of the parts that the file is chopped in
	 * @param parallelDownloads
	 *            the number of maximum concurrent connections to other clients
	 *            for the download of this file
	 * @param executor
	 *            the Executor to execute all threads
	 */
	public LoadbalancerMainThread(Vector<UserInfo> owningUsers, String checksum, String localPath, long fileSize,
			long chunksize, int parallelDownloads, Executor executor) {
		this.chunksize = chunksize;
		this.checksum = checksum;
		this.localPath = localPath;
		this.parallelDownloads = parallelDownloads;
		this.chunkThreads = new Vector<ProgressInfo>(parallelDownloads);
		fileName = this.localPath.substring(this.localPath.lastIndexOf("/") + 1, this.localPath.length());
		this.owningUsers = owningUsers;
		loadComparator = new LoadComparator();
		fileAssembler = new FileAssembler(tmpDirectory + fileName, localPath, (int) (fileSize / chunksize) + 1,
				chunkThreads);
		executor.execute(fileAssembler);
	}

	@Override
	public void run() {
		transmittedData = 0;
		long dataToSend = 0;
		chunknumber = 0;
		lastStartIndex = 0;
		while (transmittedData < fileSize) {
			if (chunkThreads.size() < parallelDownloads || lastStartIndex == fileSize) {
				if (lastStartIndex + chunksize > fileSize) {
					dataToSend = fileSize - lastStartIndex;
				} else {
					dataToSend = chunksize;
				}
				startThread(lastStartIndex, dataToSend, chunknumber);
				chunknumber++;
				checkFinishedThreads();
			} else {
				try {
					Thread.sleep(SLEEP_TIME);
					checkFinishedThreads();
				} catch (InterruptedException e) {
					// TODO error handling
				}
			}
		}
	}

	/**
	 * sorts the list by the load of the users and starts the download of a
	 * chunk from the file. - also adds the info about which chunk and thread
	 * and stuff into the list of threads
	 * 
	 * @param startIndex
	 *            the index from which to start downloading the file
	 * @param dataToSend
	 *            the number of bytes to pull
	 * @param chunknumber
	 *            which part of the file, counting starts at 0
	 */
	private void startThread(long startIndex, long dataToSend, int chunknumber) {
		Collections.sort(owningUsers, loadComparator);
		FileConnectionThread fileConnectionThread = new FileConnectionThread(owningUsers.get(0), checksum, fileSize,
				tmpDirectory + fileName + chunknumber, startIndex, startIndex + dataToSend);
		executor.execute(fileConnectionThread);
		chunkThreads.add(new ProgressInfo(startIndex, startIndex + dataToSend, chunknumber,
				tmpDirectory + fileName + chunknumber, fileConnectionThread));
	}

	/**
	 * loops through all threads in the list and checks if they have finished -
	 * if they have, handleFinishedThread is called - if they have not finished
	 * and the thread is dead restarts downloading the thread by calling the
	 * startThread function with the info of the failed thread
	 */
	private void checkFinishedThreads() {
		for (ProgressInfo progressInfo : chunkThreads) {
			if (progressInfo.isFinishedSuccessfully()) {
				handleFinishedThread(progressInfo);
			} else if (progressInfo.getFileConnectionThread() == null
					|| !progressInfo.getFileConnectionThread().isAlive()) {
				startThread(progressInfo.getStartBlock(), progressInfo.getEndBlock() - progressInfo.getStartBlock(),
						progressInfo.getChunknumber());
				chunkThreads.remove(progressInfo);
			}
		}
	}

	/**
	 * sets the chunk ready to be added to the main file in the FileAssembler.
	 * Also updates the transmitted data
	 * 
	 * @param progressInfo
	 *            the progressInfo whose thread finished the download of its
	 *            chunk
	 */
	private void handleFinishedThread(ProgressInfo progressInfo) {
		fileAssembler.setChunkReady(progressInfo.getChunknumber(),
				progressInfo.getEndBlock() - progressInfo.getStartBlock());
		transmittedData += (progressInfo.getEndBlock() - progressInfo.getStartBlock());
	}

}
