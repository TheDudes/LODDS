package studyproject.API.Lvl.Mid;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;

import studyproject.API.Core.Timestamp;
import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Core.File.InfoList.FileInfoListType;
import studyproject.API.Lvl.Low.Handles;
import studyproject.API.Lvl.Low.Requests;
import studyproject.API.Lvl.Mid.Core.RemoteFileInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class UpdateFileInfoThread extends Thread {

	private UserInfo userConnectionInfo;
	private LODDS loddsObject;

	public UpdateFileInfoThread(UserInfo userConnectionInfo, LODDS loddsObject) {
		this.userConnectionInfo = userConnectionInfo;
		this.loddsObject = loddsObject;
	}

	@Override
	public void run() {
		try (Socket socket = new Socket(userConnectionInfo.getIpAddress(), userConnectionInfo.getPort());
				BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(socket.getOutputStream());
				BufferedReader bufferedreader = new BufferedReader(new InputStreamReader(socket.getInputStream()))) {
			ArrayList<FileInfo> fileInfoList = new ArrayList<FileInfo>();
			Timestamp newFileListTimestamp = new Timestamp();
			FileInfoListType infoType = new FileInfoListType();
			Requests.getInfo(bufferedOutputStream, userConnectionInfo.getLastUpdate());
			Handles.handleInfo(bufferedreader, fileInfoList, newFileListTimestamp, infoType);
			insertEntries(fileInfoList);
		} catch (IOException e) {
			// TODO error handling
		}
	}
	
	private void insertEntries(ArrayList<FileInfo> fileInfoList){
		ConcurrentHashMap<String, RemoteFileInfo> availableFiles = loddsObject.getAvailableFiles();
		for(FileInfo fileInfo: fileInfoList){
			if(fileInfo.fileAction.equals(FileAction.add)){
				if(availableFiles.containsKey(fileInfo.checksum)){
					if(!availableFiles.get(fileInfo.checksum).getOwningUsers()
							.contains(userConnectionInfo)){
						availableFiles.get(fileInfo.checksum).getOwningUsers().add(userConnectionInfo);
					}
				} else{
				}
			}
		}
	}

}
