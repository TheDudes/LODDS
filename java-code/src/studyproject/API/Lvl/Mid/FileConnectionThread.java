package studyproject.API.Lvl.Mid;

import java.io.*;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;

import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleStringProperty;
import studyproject.API.Errors.ErrorFactory;
import studyproject.API.Loadbalancer.ProgressInfo;
import studyproject.API.Lvl.Low.Handles;
import studyproject.API.Lvl.Low.Requests;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.API.Lvl.Mid.ThreadMonitoring.MonitoredThread;
import studyproject.logging.LogKey;

/**
 * Thread to get a file from another client After creating a new instance of
 * this class call start() and the file specified in the constructor will be
 * transmitted in the way specified in the specification
 * 
 * @author Michael
 *
 */
public class FileConnectionThread extends Thread implements MonitoredThread {

	private UserInfo user;
	private String checksum;
	private String localPath;
	private Logger logger = Logger.getGlobal();
	private long size;
	private long endIndex = 0;
	private boolean supportLoadbalancing = false;
	private FileOutputStream fileOutStream;
	private boolean submitted;
	ProgressInfo progressInfo;
	private final long chunksize = 1 << 21;
	private SimpleLongProperty startIndex = new SimpleLongProperty(0L);
	private SimpleStringProperty currentFile = new SimpleStringProperty("");
	private SimpleDoubleProperty progress = new SimpleDoubleProperty(0.0);
	private boolean oneOfMultiple = false;

	/**
	 * Use this constructor if you want to pull the whole file, if you only want
	 * to pull part of a file use the constructor with start and endIndex
	 * 
	 * @param user
	 *            the user from which to pull the file
	 * 
	 * @param checksum
	 *            the checksum of the file that should be pulled
	 * 
	 * @param size
	 *            the size of the file that should be pulled
	 * 
	 * @param localPath
	 *            the complete path on which the file should be saved
	 */
	public FileConnectionThread(UserInfo user, String checksum, long size, String localPath) {
		this.user = user;
		this.checksum = checksum;
		this.localPath = localPath;
		this.size = size;
		this.endIndex = size;
		currentFile.setValue(localPath.substring(localPath.lastIndexOf("/") + 1));
	}

	/**
	 * 
	 * This constructor is used if you only want to pull a part of the file, the
	 * part is specified by the start and endIndex
	 * 
	 * @param user
	 *            the user from which to pull the file
	 * 
	 * @param checksum
	 *            the checksum of the file that should be pulled
	 * 
	 * @param size
	 *            the size of the file that should be pulled
	 * 
	 * @param localPath
	 *            the complete path on which the file should be saved
	 * 
	 * @param startIndex
	 *            the index from which to start the transmission
	 * 
	 * @param endIndex
	 *            the index on which to end the transmission
	 */
	public FileConnectionThread(UserInfo user, String checksum, long size, String localPath, long startIndex,
			long endIndex) {
		this.user = user;
		this.checksum = checksum;
		this.localPath = localPath;
		this.startIndex = new SimpleLongProperty(startIndex);
		this.endIndex = endIndex;
		this.size = size;
		currentFile.setValue(localPath.substring(localPath.lastIndexOf("/") + 1));

	}

	/**
	 * 
	 * This constructor is used if you only want to pull a part of the file, the
	 * part is specified by the start and endIndex. This constructor is used if
	 * this thread is part of loadbalancing
	 * 
	 * @param user
	 *            the user from which to pull the file
	 * 
	 * @param checksum
	 *            the checksum of the file that should be pulled
	 * 
	 * @param size
	 *            the size of the file that should be pulled
	 * 
	 * @param localPath
	 *            the complete path on which the file should be saved
	 * 
	 * @param startIndex
	 *            the index from which to start the transmission
	 * 
	 * @param endIndex
	 *            the index on which to end the transmission
	 * 
	 * @param progressInfo
	 *            the information about this thread if this thread is called via
	 *            loadbalancing
	 */
	public FileConnectionThread(UserInfo user, String checksum, long size, String localPath, long startIndex,
			long endIndex, ProgressInfo progressInfo) {
		this.user = user;
		this.checksum = checksum;
		this.localPath = localPath;
		this.startIndex = new SimpleLongProperty(startIndex);
		this.endIndex = endIndex;
		this.size = size;
		supportLoadbalancing = true;
		this.progressInfo = progressInfo;
		currentFile.setValue(localPath.substring(localPath.lastIndexOf("/") + 1));

	}

	@Override
	/**
	 * starts pulling the file with the parameters set in the constructor
	 */
	public void run() {
		logger.log(ErrorFactory.build(Level.INFO, LogKey.filetransferInit,
				"Filetransfer initiated to user: '" + user.toString() + "' for file '" + checksum + "'"));
		int returnValue;
		try (Socket socket = new Socket(user.getIpAddress(), user.getPort());
				BufferedOutputStream outStream = new BufferedOutputStream(socket.getOutputStream());
				BufferedInputStream inStream = new BufferedInputStream(socket.getInputStream())) {
			File file = new File(localPath);
			if (file.getParentFile() != null) {
				file.getParentFile().mkdirs();
			}
			fileOutStream = new FileOutputStream(file);

			if ((returnValue = Requests.getFile(outStream, checksum, startIndex.get(), endIndex)) != 0) {
				logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, returnValue));
				return;
			}
			logger.log(ErrorFactory.build(Level.INFO, LogKey.getFile,
					"Sent getFile to user '" + user.toString() + "' for file '" + checksum + "'"));
			while (startIndex.get() != endIndex) {
				if (startIndex.get() + chunksize < endIndex) {
					returnValue = Handles.handleFile(inStream, fileOutStream, chunksize);
					startIndex.setValue(startIndex.get() + chunksize);
				} else {
					returnValue = Handles.handleFile(inStream, fileOutStream, endIndex - startIndex.get());
					startIndex.setValue(endIndex);
				}
				progress.setValue((double) (startIndex.get()) / (double) size);
				if (returnValue != 0) {
					logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, returnValue));
				} else {
					if (supportLoadbalancing) {
						progressInfo.setFinishedSuccessfully(true);
					}
				}
			}
			fileOutStream.close();
		} catch (IOException e) {
			logger.log(ErrorFactory.build(Level.SEVERE, LogKey.error, "IOException thrown: ", e));
		}

		logger.log(ErrorFactory.build(Level.INFO, LogKey.filetransferComplete,
				"Download of '" + checksum + "' from user '" + user.toString() + " 'finished"));
	}

	@Override
	public boolean isSubmitted() {
		return submitted;
	}

	@Override
	public void setSubmitted(boolean toSet) {
		submitted = toSet;
	}

	@Override
	public synchronized void setProgress(SimpleDoubleProperty toSet) {
		this.progress = toSet;
	}

	@Override
	public synchronized SimpleDoubleProperty getProgress() {
		return progress;
	}

	@Override
	public synchronized SimpleLongProperty getDoneSize() {
		return startIndex;
	}

	@Override
	public long getWholeSize() {
		return size;
	}

	@Override
	public synchronized SimpleStringProperty getCurrentFileName() {
		return currentFile;
	}

	@Override
	public synchronized boolean isOneOfMultiple() {
		return oneOfMultiple;
	}

	@Override
	public void setOneOfMultiple(boolean oneOfMultiple) {
		this.oneOfMultiple = oneOfMultiple;
	}

	@Override
	public String getNameToDisplay() {
		return localPath.substring(localPath.lastIndexOf("/") + 1);
	}
}
