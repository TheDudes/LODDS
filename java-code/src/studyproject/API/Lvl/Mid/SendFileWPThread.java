package studyproject.API.Lvl.Mid;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.Socket;
import java.nio.file.Paths;
import java.util.logging.Level;
import java.util.logging.Logger;

import studyproject.API.Core.File.FileInfo;
import studyproject.API.Errors.ErrorFactory;
import studyproject.API.Lvl.Low.Handles;
import studyproject.API.Lvl.Low.Requests;
import studyproject.API.Lvl.Low.Responses;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.logging.LogKey;

/**
 * This Thread is used to send a file with permission, which further means this
 * abstracts the low lvl api functions getSendPermission, and
 * handleSendPermission, which abstracts the protocols' "get send-permission".
 * Use Thread.start() to start the run function, which will then send a request
 * to send a file, if the other client responds with an "OK" the file specified
 * with and hash is sent via the opened socket.
 * 
 * @author ninti
 *
 */
public class SendFileWPThread extends Thread {

	private UserInfo user;
	private FileInfo fileInfo;
	private long timeout;
	private Logger logger = Logger.getGlobal();

	/**
	 * Constructor for an new SendFileWithPermissionThread
	 * 
	 * @param user
	 *            the user to whom the file shall be sent
	 * @param fileInfo
	 *            the {@link FileInfo} which represents the file to send
	 * @param timeout
	 *            the time in ms until the request times out
	 */
	public SendFileWPThread(UserInfo user, long timeout, FileInfo fileInfo) {
		this.user = user;
		this.fileInfo = fileInfo;
		this.timeout = timeout;
	}

	@Override
	public void run() {
		try (Socket socket = new Socket(user.getIpAddress(), user.getPort());
				BufferedOutputStream outStream = new BufferedOutputStream(socket.getOutputStream());
				FileInputStream fileInStream = new FileInputStream(
						(Paths.get(fileInfo.parentDirectory).resolve(fileInfo.fileName)).toString())) {
			int errorCode;
			if ((errorCode = Requests.getSendPermission(outStream, fileInfo.size, timeout,
					new File(fileInfo.fileName).getName())) != 0) {
				logger.log(ErrorFactory.build(Level.INFO, LogKey.getSendPermission, errorCode));
			}
			errorCode = Handles.handleSendPermission(socket, timeout);
			if (errorCode == 0) {
				errorCode = Responses.respondFile(outStream, fileInStream, 0, fileInfo.size);
				if (errorCode != 0) {
					logger.log(ErrorFactory.build(Level.SEVERE, LogKey.filetransferInit, errorCode));
				}
			} else {

				logger.log(ErrorFactory.build(Level.INFO, LogKey.handleSendPermission, errorCode));
			}
		} catch (IOException e) {
			logger.log(ErrorFactory.build(Level.WARNING, LogKey.getSendPermission, e));
		}
	}
}
