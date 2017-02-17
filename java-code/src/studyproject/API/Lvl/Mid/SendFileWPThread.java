package studyproject.API.Lvl.Mid;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.Socket;
import java.nio.file.Paths;
import java.util.logging.Level;

import studyproject.API.Core.File.FileInfo;
import studyproject.API.Errors.ErrLog;
import studyproject.API.Lvl.Low.Handles;
import studyproject.API.Lvl.Low.Requests;
import studyproject.API.Lvl.Low.Responses;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.logging.APILvl;
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
				ErrLog.log(Level.INFO, LogKey.getSendPermission, APILvl.mid, errorCode,
						getClass().getName() + ".run() : " + this.getId());
			}
			errorCode = Handles.handleSendPermission(socket, timeout);
			if (errorCode == 0) {
				errorCode = Responses.respondFile(outStream, fileInStream, 0, fileInfo.size);
				if (errorCode != 0) {
					ErrLog.log(Level.SEVERE, LogKey.filetransferInit, APILvl.mid, errorCode,
							getClass().getName() + ".run() : " + this.getId());
				}
			} else {

				ErrLog.log(Level.INFO, LogKey.handleSendPermission, APILvl.mid, errorCode,
						getClass().getName() + ".run() : " + this.getId());
			}
		} catch (IOException e) {
			ErrLog.log(Level.WARNING, LogKey.getSendPermission, APILvl.mid,
					getClass().getName() + ".run() : " + this.getId(), e.getStackTrace().toString());
		}
	}
}
