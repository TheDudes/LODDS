package studyproject.API.Lvl.Mid;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.Socket;

import studyproject.API.Lvl.Low.Handles;
import studyproject.API.Lvl.Low.Requests;
import studyproject.API.Lvl.Mid.Core.UserInfo;

/**
 * Thread to get a file from another client After creating a new instance of
 * this class call start() and the file specified in the constructor will be
 * transmitted in the way specified in the specification
 * 
 * @author Michael
 *
 */
public class FileConnectionThread extends Thread {

	private UserInfo user;
	private String checksum;
	private String localPath;
	private long size;
	private long startIndex = 0;
	private long endIndex = 0;

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
			long endIndex){
		this.user = user;
		this.checksum = checksum;
		this.localPath = localPath;
		this.startIndex = startIndex;
		this.endIndex = endIndex;
		this.size = size;
	}

	@Override
	/**
	 * starts pulling the file with the parameters set in the constructor
	 */
	public void run() {
		int returnValue;
		try (Socket socket = new Socket(user.getIpAddress(), user.getPort());
				BufferedOutputStream outStream = new BufferedOutputStream(socket.getOutputStream());
				BufferedInputStream inStream = new BufferedInputStream(socket.getInputStream());
				FileOutputStream fileOutStream = new FileOutputStream(new File(localPath))) {
			//whole file?
			if (endIndex == 0) {
				returnValue = Requests.getFile(outStream, checksum, startIndex, size);
			} else {
				returnValue = Requests.getFile(outStream, checksum, startIndex, endIndex);
			}
			if (returnValue != 0) {
				// TODO error handling
			} else {
			}
			//whole file?
			if (endIndex == 0) {
				returnValue = Handles.handleFile(inStream, fileOutStream, size);
			} else {
				returnValue = Handles.handleFile(inStream, fileOutStream, endIndex);
			}
			if (returnValue != 0) {
				// TODO error handling
			}
		} catch (IOException e) {
			// TODO error handling
		}
	}

}
