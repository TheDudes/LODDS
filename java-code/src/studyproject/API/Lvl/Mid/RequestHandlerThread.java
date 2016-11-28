package studyproject.API.Lvl.Mid;

import java.io.IOException;
import java.io.InputStream;
import java.net.ServerSocket;
import java.net.Socket;

//import studyproject.API.Core.Request.GetFileRequest;
//import studyproject.API.Core.Request.GetInfoRequest;
//import studyproject.API.Core.Request.GetPermissionRequest;
import studyproject.API.Core.Request.RequestContainer;
import studyproject.API.Lvl.Low.RequestHandler;
import studyproject.API.Lvl.Mid.Lodds.Lodds;

public class RequestHandlerThread extends Thread {

	Lodds loddsObj;
	ServerSocket serverSocket;
	Socket socket;
	private boolean run;

	/**
	 * RequestThread constructor
	 * 
	 * @param loddsObj
	 *            Instance of the LODDS-Class
	 * @param socket
	 *            ServerSocket to accept incoming requests
	 */
	public RequestHandlerThread(Lodds loddsObj, ServerSocket socket) {
		this.loddsObj = loddsObj;
		this.serverSocket = socket;
		run = true;
	}

	/**
	 * Start thread and handle incoming requests
	 */
	@Override
	public void run() {
		RequestContainer reqContainer = new RequestContainer();
		InputStream socketStream;
		try {
			socket = serverSocket.accept();
			socketStream = socket.getInputStream();
		} catch (IOException e) {
			// TODO Errorhandling Mid LVL
			e.printStackTrace();
			return;
		}
		while (run) {
			if (RequestHandler.parseRequest(socketStream, reqContainer) != 0) {
				// TODO Errorhandling: wrong return value
				continue;
			}
			switch (reqContainer.request.getType()) {
			case GET_FILE:
				// GetFileRequest fileReq = (GetFileRequest)
				// reqContainer.request;
				// loddsObj.sendFile(loddsObj.getUserName(), fileReq.checksum,
				// fileReq.startIndex, fileReq.endIndex);
				break;
			case GET_INFO:
				// GetInfoRequest infoReq = (GetInfoRequest)
				// reqContainer.request;
				// // loddsObj.
				break;
			case GET_SEND_PERMISSION:
				// GetPermissionRequest permissionReq = (GetPermissionRequest)
				// reqContainer.request;
				// loddsObj.sendFileWP(loddsObj.getUserName(),
				// permissionReq.checksum);
				break;
			}
		}
	}

	/**
	 * Set run variable to false to stop the RequestThread
	 */
	public void setThread() {
		run = false;
	}
}
