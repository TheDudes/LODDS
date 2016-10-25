package studyproject.API.Lvl.Mid;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.nio.file.Paths;

import studyproject.API.Core.File.FileInfo;
import studyproject.API.Lvl.Low.Handles;
import studyproject.API.Lvl.Low.Requests;
import studyproject.API.Lvl.Low.Responses;
import studyproject.API.Lvl.Mid.Core.UserInfo;

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
				BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
				FileInputStream fileInStream = new FileInputStream(
						(Paths.get(fileInfo.parentDirectory).resolve(fileInfo.fileName)).toString());) {
			if (Requests.getSendPermission(outStream, fileInfo.size, timeout, fileInfo.fileName) == 1)
				;
			// TODO error handling
			if (Handles.handleSendPermission(reader, timeout) == 0) {
				if (Responses.respondFile(outStream, fileInStream, 0, fileInfo.size) != 0)
					;
				// TODO error handling

			}
		} catch (IOException e) {
			// TODO error handling
		}
	}
}
