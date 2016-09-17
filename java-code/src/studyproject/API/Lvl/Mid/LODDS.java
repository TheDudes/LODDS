package studyproject.API.Lvl.Mid;

import java.util.ArrayList;
import java.util.Vector;

import studyproject.API.Lvl.Mid.Core.ConnectionInfo;
import studyproject.API.Lvl.Mid.Core.FileChange;
import studyproject.API.Lvl.Mid.Core.RemoteFileInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class LODDS {
	
	private Vector<FileChange> localFileChanges;
	private Vector<UserInfo> clientList;
	private Vector<RemoteFileInfo> availableFiles;
	private long load;
	private String interfaceName;
	private int advertisePort;
	private int listenPort;
	private String userName;

	public void startAdvertising(){
	}
	
	public void stopAdvertising(){
	}
	
	public void startListening(){
	}
	
	public void stopListening(){
	}
	
	public void getFile(String user, String checksum, String localPath){
		
	}
	
	public UserInfo getUserConnectionInfo(String user){
		return null;
	}
	
	public int shareFolder(String path){
		return 0;
	}
	
	public int unshareFolder(String path){
		return 0;
	}
	
	public ArrayList<String> getSharedFolders(){
		return null;
	}
	
	public Vector<ConnectionInfo> getOutstandingBytes(){
		return null;
	}
	
	public Vector<FileChange> getFileChanges(long timeStamp){
		return null;
	}
	
	public Vector<UserInfo> getUsers(){
		return clientList;
	}
	
	public void setInterface(String interfaceName){
		this.interfaceName = interfaceName;
	}
	
	public String getInterface(){
		return interfaceName;
	}
	
	public void setAdvertisePort(int port){
		this.advertisePort = port;
	}
	
	public int getAdvertisePort(){
		return advertisePort;
	}
	
	public void setListenPort(int port){
		this.listenPort = port;
	}
	
	public int getListenPort(){
		return listenPort;
	}
	
	public void setUserName(String userName){
		this.userName = userName;
	}
	
}
