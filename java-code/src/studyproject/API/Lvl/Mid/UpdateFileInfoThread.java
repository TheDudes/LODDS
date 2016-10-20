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

/**
 * Thread that asks a specified user for an update to the list of shared files
 * the user has. It then receives a list with changes and applies those to the
 * list the user object has and to the hashmap of the lodds object that contains
 * all shared files
 * 
 * @author Michael
 *
 */
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
			userConnectionInfo.setFileListTimestamp(newFileListTimestamp.value);
			userConnectionInfo.setLastUpdate(System.currentTimeMillis() / 1000);
			updateHashmapEntries(fileInfoList);
			updateClientFileList(fileInfoList);
		} catch (IOException e) {
			// TODO error handling
		}
	}

	/**
	 * updates the list contained in the user object with the changes from the
	 * list
	 * 
	 * @param fileInfoList
	 *            the list of changes
	 */
	private void updateClientFileList(ArrayList<FileInfo> fileInfoList) {
		for (FileInfo fileInfo : fileInfoList) {
			if (fileInfo.fileAction.equals(FileAction.add)) {
				userConnectionInfo.getFileList().add(new RemoteFileInfo(fileInfo));
			} else if (fileInfo.fileAction.equals(FileAction.del)) {
				userConnectionInfo.getFileList().remove(new RemoteFileInfo(fileInfo));
			}
		}
	}

	/**
	 * applies the changes passed in the fileInfoList to the hashmap that
	 * contains all shared files
	 * 
	 * @param fileInfoList
	 *            the list of changes
	 */
	private void updateHashmapEntries(ArrayList<FileInfo> fileInfoList) {
		ConcurrentHashMap<String, RemoteFileInfo> availableFiles = loddsObject.getAvailableFiles();
		for (FileInfo fileInfo : fileInfoList) {
			if (fileInfo.fileAction.equals(FileAction.add)) {
				if (availableFiles.containsKey(fileInfo.checksum)) {
					if (!availableFiles.get(fileInfo.checksum).getOwningUsers().contains(userConnectionInfo)) {
						availableFiles.get(fileInfo.checksum).getOwningUsers().add(userConnectionInfo);
					}
				} else {
					availableFiles.put(fileInfo.checksum, new RemoteFileInfo(fileInfo, userConnectionInfo));
				}
			} else if (fileInfo.fileAction.equals(FileAction.del)) {
				if (availableFiles.get(fileInfo.checksum).getOwningUsers().size() > 1) {
					availableFiles.get(fileInfo.checksum).getOwningUsers().remove(userConnectionInfo);
				} else if (availableFiles.get(fileInfo.checksum).getOwningUsers().contains(userConnectionInfo)) {
					availableFiles.remove(fileInfo.checksum);
				}
			}
		}
	}

}
