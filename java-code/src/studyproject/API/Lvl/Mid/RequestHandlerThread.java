package studyproject.API.Lvl.Mid;

import java.io.IOException;
import java.io.InputStream;
import java.net.ServerSocket;
import java.net.Socket;

import studyproject.API.Core.File.FileInfo;
import studyproject.API.Core.Request.GetFileRequest;
import studyproject.API.Core.Request.GetInfoRequest;
import studyproject.API.Core.Request.GetPermissionRequest;
import studyproject.API.Core.Request.RequestContainer;
import studyproject.API.Lvl.Low.RequestHandler;
import studyproject.API.Lvl.Mid.Lodds.Lodds;
import studyproject.API.Lvl.Mid.ThreadMonitoring.ThreadExecutor;

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
				GetFileRequest fileReq = (GetFileRequest) reqContainer.request;
				FileInfo fileInfo = loddsObj.getWatchService().getFileByChecksum(fileReq.checksum);
				FileSenderThread fileSenderThread = new FileSenderThread(socket, fileInfo, fileReq.startIndex, fileReq.endIndex);
				threadExecutor.execute(fileSenderThread);
				break;
			case GET_INFO:
				GetInfoRequest infoReq = (GetInfoRequest) reqContainer.request;
				InfoSenderThread infoSenderThread = new InfoSenderThread(socket, loddsObj.getWatchService(), infoReq.timestamp);
				threadExecutor.execute(infoSenderThread);
				break;
			case GET_SEND_PERMISSION:				
				// TODO Ask user to receive a file via GUI window
				GetPermissionRequest permissionReq = (GetPermissionRequest) reqContainer.request;
				// TODO Ask user where to save the file and under which name via 'save as' GUI
				// TODO Split the returning path into path and filename, save them in the two variables
				String pathToSave = "";
				String fileName = permissionReq.fileName;
				GetFileWPThread getFileThread = new GetFileWPThread(socket, pathToSave, fileName, permissionReq.fileSize);
				threadExecutor.execute(getFileThread);
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
