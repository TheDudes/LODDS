package studyproject.API.Lvl.Mid;

import java.util.ArrayList;
import java.util.Vector;

import studyproject.API.Lvl.Mid.Core.ConnectionInfo;
import studyproject.API.Lvl.Mid.Core.FileChange;
import studyproject.API.Lvl.Mid.Core.UserInfo;

public class LODDS {

	public void startAdvertising(){
	}
	
	public void stopAdvertising(){
	}
	
	public void startListening(){
	}
	
	public void stopListening(){
	}
	
	public ArrayList<UserInfo> getUsers(){
		return null;
	}
	
	public void setInterface(String interfaceName){
		
	}
	
	public String getInterface(){
		return null;
	}
	
	public void setAdvertisePort(int port){
		
	}
	
	public int getAdvertisePort(){
		return 0;
	}
	
	public void setListenPort(int port){
		
	}
	
	public int getListenPort(){
		return 0;
	}
	
	public void setUserName(String userName){
		
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
	
}
