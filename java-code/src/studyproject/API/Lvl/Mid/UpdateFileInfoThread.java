package studyproject.API.Lvl.Mid;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;

import studyproject.API.Core.Timestamp;
import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Core.File.InfoList.FileInfoListType;
import studyproject.API.Core.File.InfoList.InfoType;
import studyproject.API.Lvl.Low.Handles;
import studyproject.API.Lvl.Low.Requests;
import studyproject.API.Lvl.Mid.Core.FileCoreInfo;
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

	private UserInfo userInfo;

	public UpdateFileInfoThread(UserInfo userConnectionInfo) {
		this.userInfo = userConnectionInfo;
	}

	@Override
	public void run() {
		try (Socket socket = new Socket(userInfo.getIpAddress(),
				userInfo.getPort());
				BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(
						socket.getOutputStream());
				BufferedReader bufferedreader = new BufferedReader(
						new InputStreamReader(socket.getInputStream()))) {
			ArrayList<FileInfo> fileInfoList = new ArrayList<FileInfo>();
			Timestamp newFileListTimestamp = new Timestamp();
			FileInfoListType infoType = new FileInfoListType();
			Requests.getInfo(bufferedOutputStream, userInfo.getLastUpdate());
			Handles.handleInfo(bufferedreader, fileInfoList,
					newFileListTimestamp, infoType);
			userInfo.setFileListTimestamp(newFileListTimestamp.value);
			userInfo.setLastUpdate(System.currentTimeMillis() / 1000);
			if (infoType.equals(InfoType.all)) {
				userInfo.setChecksumToPath(new ConcurrentHashMap<String, Vector<String>>());
				userInfo.setPathToFileInfo(new ConcurrentHashMap<String, FileCoreInfo>());
			}
			updateEntries(fileInfoList);
		} catch (IOException e) {
			// TODO error handling
		}
	}

	/**
	 * updates the hash maps contained in the user object with the changes from
	 * the list
	 * 
	 * @param fileInfoList
	 *            the list of changes
	 */
	private void updateEntries(ArrayList<FileInfo> fileInfoList) {
		for (FileInfo fileInfo : fileInfoList) {
			if (fileInfo.fileAction.equals(FileAction.add)) {
				addEntry(fileInfo);
			} else if (fileInfo.fileAction.equals(FileAction.del)) {
				deleteEntry(fileInfo);
			}
		}
	}

	private void deleteEntry(FileInfo fileInfo) {
		if (userInfo.getChecksumToPath().containsKey(fileInfo.checksum)) {
			if (userInfo.getChecksumToPath().get(fileInfo.checksum).size() > 1) {
				userInfo.getChecksumToPath().get(fileInfo.checksum)
						.remove(fileInfo.fileName);
			} else {
				userInfo.getChecksumToPath().remove(fileInfo.checksum);
			}
		}
		userInfo.getPathToFileInfo().remove(fileInfo.fileName);
	}

	private void addEntry(FileInfo fileInfo) {
		if (!userInfo.getChecksumToPath().containsKey(fileInfo.checksum)) {
			userInfo.getChecksumToPath().get(fileInfo.checksum)
					.add(fileInfo.fileName);
		} else if (!userInfo.getChecksumToPath().get(fileInfo.checksum)
				.contains(fileInfo.fileName)) {
			Vector<String> pathList = new Vector<String>();
			pathList.add(fileInfo.fileName);
			userInfo.getChecksumToPath().put(fileInfo.checksum, pathList);
		}
		userInfo.getPathToFileInfo().put(fileInfo.fileName,
				new FileCoreInfo(fileInfo.checksum, fileInfo.size));
	}

}
