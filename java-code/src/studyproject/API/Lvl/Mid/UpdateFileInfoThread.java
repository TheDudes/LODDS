package studyproject.API.Lvl.Mid;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;

import studyproject.API.Core.Timestamp;
import studyproject.API.Core.File.FileAction;
import studyproject.API.Core.File.FileInfo;
import studyproject.API.Core.File.InfoList.FileInfoListType;
import studyproject.API.Core.File.InfoList.InfoType;
import studyproject.API.Errors.ErrLog;
import studyproject.API.Lvl.Low.Handles;
import studyproject.API.Lvl.Low.Requests;
import studyproject.API.Lvl.Mid.Core.FileCoreInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.logging.APILvl;
import studyproject.logging.LogKey;

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
		try (Socket socket = new Socket(userInfo.getIpAddress(), userInfo.getPort());
				BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(socket.getOutputStream());
				BufferedReader bufferedreader = new BufferedReader(new InputStreamReader(socket.getInputStream()))) {
			ArrayList<FileInfo> fileInfoList = new ArrayList<FileInfo>();
			Timestamp newFileListTimestamp = new Timestamp();
			FileInfoListType infoType = new FileInfoListType();
			int err = Requests.getInfo(bufferedOutputStream, userInfo.getLastUpdate());
			if (err != 0)
				ErrLog.log(Level.SEVERE, LogKey.info, APILvl.mid, err, "Requests.getInfo");
			ErrLog.log(Level.INFO, LogKey.info, APILvl.mid, "Requests.getInfo",
					"Get Info " + userInfo.getUserName() + " timestamp: " + userInfo.getLastUpdate());
			err = Handles.handleInfo(bufferedreader, fileInfoList, newFileListTimestamp, infoType);
			if (err != 0) {
				ErrLog.log(Level.SEVERE, LogKey.info, APILvl.mid, err, "Handles.handleInfo");
				// TODO what shall happen with the thread if the handleInfo
				// failed
			}
			userInfo.setLastUpdate(newFileListTimestamp.value);
			if (infoType.equals(InfoType.all)) {
				userInfo.setChecksumToPath(new ConcurrentHashMap<String, Vector<String>>());
				userInfo.setPathToFileInfo(new ConcurrentHashMap<String, FileCoreInfo>());
			}
			updateEntries(fileInfoList);
		} catch (IOException e) {
			ErrLog.log(Level.SEVERE, LogKey.error, APILvl.mid, getClass().getName() + "run()",
					"IOException thrown: " + e.getStackTrace());
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
				userInfo.getChecksumToPath().get(fileInfo.checksum).remove(fileInfo.fileName);
			} else {
				userInfo.getChecksumToPath().remove(fileInfo.checksum);
			}
		}
		userInfo.getPathToFileInfo().remove(fileInfo.fileName);
	}

	private void addEntry(FileInfo fileInfo) {
		if (!userInfo.getChecksumToPath().containsKey(fileInfo.checksum)) {
			Vector<String> pathList = new Vector<String>();
			pathList.add(fileInfo.fileName);
			userInfo.getChecksumToPath().put(fileInfo.checksum, pathList);
		} else if (!userInfo.getChecksumToPath().get(fileInfo.checksum).contains(fileInfo.fileName)) {
			userInfo.getChecksumToPath().get(fileInfo.checksum).add(fileInfo.fileName);
		}
		userInfo.getPathToFileInfo().put(fileInfo.fileName,
				new FileCoreInfo(fileInfo.checksum, fileInfo.size, fileInfo.fileName));
	}

}
