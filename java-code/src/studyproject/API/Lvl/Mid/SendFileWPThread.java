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

public class SendFileWPThread extends Thread {

	private UserInfo user;
	private long timeout;
	private FileInfo fileInfo;

	public SendFileWPThread(UserInfo user, long timeout, FileInfo fileInfo) {
		this.user = user;
		this.timeout = timeout;
		this.fileInfo = fileInfo;
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
			if (Handles.handleSendPermission(reader) == 0) {
				if (Responses.respondFile(outStream, fileInStream, 0, fileInfo.size) != 0)
					;
				// TODO error handling

			}
		} catch (IOException e) {
			// TODO error handling
		}
	}
}
