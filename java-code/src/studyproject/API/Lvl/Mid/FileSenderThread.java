package studyproject.API.Lvl.Mid;

import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.Socket;
import java.nio.file.Paths;
import java.util.logging.Level;
import java.util.logging.Logger;

import studyproject.API.Core.File.FileInfo;
import studyproject.API.Errors.ErrorFactory;
import studyproject.API.Lvl.Low.Responses;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.logging.LogKey;

/**
 * This Thread class should be used to send a file partly or whole to the User
 * specified with the {@link UserInfo}. Create a new instance of this class and
 * call the run function to start the transaction
 * 
 * @author ninti
 *
 */
public class FileSenderThread extends Thread {
	private long size;
	private long startIndex = 0;
	private long endIndex = 0;
	private FileInfo fileInfo;
	private Socket socket;
	private Logger logger = Logger.getGlobal();

	/**
	 * This constructor is used to send a whole file to the specified user
	 * 
	 * @param socket
	 *            the socket opened by the requester where the file shall be
	 *            sent to
	 * @param fileInfo
	 *            the fileInfo which specifies the file which shall be sent
	 */
	public FileSenderThread(Socket socket, FileInfo fileInfo) {
		this(socket, fileInfo, 0, 0);
	}

	/**
	 * This constructor is used to send a file partly to the specified user
	 * 
	 * @param socket
	 *            the socket opened by the requester where the file shall be
	 *            sent to
	 * @param fileInfo
	 *            the fileInfo which specifies the file which shall be sent
	 * @param startIndex
	 *            the start index(number of byte) where the transaction shall be
	 *            started
	 * @param endIndex
	 *            the end index(number of byte) where the transaction shall end
	 */
	public FileSenderThread(Socket socket, FileInfo fileInfo, long startIndex, long endIndex) {
		this.socket = socket;
		this.fileInfo = fileInfo;
		this.startIndex = startIndex;
		this.endIndex = endIndex;
		this.size = fileInfo.size;
	}

	@Override
	public void run() {
		int returnValue;
		try (BufferedOutputStream outStream = new BufferedOutputStream(socket.getOutputStream());
				FileInputStream fileInStream = new FileInputStream(
						(Paths.get(fileInfo.parentDirectory).resolve(fileInfo.fileName)).toString());) {

			if (endIndex == 0) {
				returnValue = Responses.respondFile(outStream, fileInStream, startIndex, size);
			} else {
				returnValue = Responses.respondFile(outStream, fileInStream, startIndex, endIndex);
			}
			if (returnValue != 0) {
				logger.log(ErrorFactory.build(Level.SEVERE, LogKey.filetransferInit, returnValue));
			}

		} catch (IOException e) {
			logger.log(ErrorFactory.build(Level.SEVERE, LogKey.filetransferInit, e));
		}
	}
}
