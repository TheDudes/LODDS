package studyproject.API.Lvl.Mid;

import java.io.IOException;
import java.io.InputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Level;

import studyproject.API.Core.File.FileInfo;
import studyproject.API.Core.Request.GetFileRequest;
import studyproject.API.Core.Request.GetInfoRequest;
import studyproject.API.Core.Request.GetPermissionRequest;
import studyproject.API.Core.Request.RequestContainer;
import studyproject.API.Errors.ErrLog;
import studyproject.API.Lvl.Low.RequestHandler;
import studyproject.API.Lvl.Mid.Lodds.Lodds;
import studyproject.API.Lvl.Mid.ThreadMonitoring.ThreadExecutor;
import studyproject.logging.APILvl;
import studyproject.logging.LogKey;

public class RequestHandlerThread extends Thread {

	Lodds loddsObj;
	ThreadExecutor threadExecutor;
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
	public RequestHandlerThread(Lodds loddsObj, ThreadExecutor threadExecutor, ServerSocket socket) {
		this.loddsObj = loddsObj;
		this.threadExecutor = threadExecutor;
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
		int errorCode;
		while (run) {
			try {
				socket = serverSocket.accept();
				socketStream = socket.getInputStream();
			} catch (IOException e) {
				try {
					socket.close();
				} catch (IOException e1) {
					ErrLog.log(Level.SEVERE, LogKey.error, APILvl.mid, getClass().getName() + "run()",
							"IOException thrown: " + e.getStackTrace());
				}
				continue;
			}
			reqContainer = new RequestContainer();
			if ((errorCode = RequestHandler.parseRequest(socketStream, reqContainer)) != 0) {
				ErrLog.log(Level.SEVERE, LogKey.error, APILvl.mid, errorCode, getClass().getName() + "run()");
				continue;
			}

			switch (reqContainer.request.getType()) {
			case GET_FILE:
				GetFileRequest fileReq = (GetFileRequest) reqContainer.request;
				FileInfo fileInfo = loddsObj.getWatchService().getFileByChecksum(fileReq.checksum);
				FileSenderThread fileSenderThread = new FileSenderThread(socket, fileInfo, fileReq.startIndex,
						fileReq.endIndex);
				threadExecutor.execute(fileSenderThread);
				break;
			case GET_INFO:
				GetInfoRequest infoReq = (GetInfoRequest) reqContainer.request;
				InfoSenderThread infoSenderThread = new InfoSenderThread(socket, loddsObj.getWatchService(),
						infoReq.timestamp);
				threadExecutor.execute(infoSenderThread);
				break;
			case GET_SEND_PERMISSION:
				GetPermissionRequest permissionReq = (GetPermissionRequest) reqContainer.request;
				permissionReq.socket = socket;
				loddsObj.getLoddsModel().getPermissionList().add(permissionReq);
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
