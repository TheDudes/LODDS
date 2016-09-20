package studyproject.API.Lvl.Mid.Core;

import java.util.Vector;

/**
 * class to store information about a file from other clients in
 * also contains a list of clients that have this specific file on
 * their disks
 * @author Michael
 *
 */
public class RemoteFileInfo {

	private String path;
	private long size;
	private String checksum;
	private Vector<UserInfo> owningUsers;
	
	/**
	 * //TODO this is the name of the file is it not? it does not have to have the same path
	 * 			on different clients 
	 * @param path
	 * 			the name of the file
	 * 
	 * @param size
	 * 			the size of the file
	 * 
	 * @param checksum
	 * 			the checksum of the file
	 */
	public RemoteFileInfo(String path, long size, String checksum){
		this.path = path;
		this.size = size;
		this.checksum = checksum;
		owningUsers = new Vector<UserInfo>();
	}
	
	/**
	 * 
	 * @return
	 * 			the name of the file
	 */
	public String getPath() {
		return path;
	}
	
	/**
	 * 
	 * @param path
	 * 			the name of the file
	 */
	public void setPath(String path) {
		this.path = path;
	}
	
	/**
	 * 
	 * @return
	 * 			the size of the file
	 */
	public long getSize() {
		return size;
	}
	
	/**
	 * 
	 * @param size
	 * 			the size of the file
	 */
	public void setSize(long size) {
		this.size = size;
	}
	
	/**
	 * 
	 * @return
	 * 			the checksum of the file
	 */
	public String getChecksum() {
		return checksum;
	}
	
	/**
	 * 
	 * @param checksum
	 * 			the checksum of the file
	 */
	public void setChecksum(String checksum) {
		this.checksum = checksum;
	}
	
	/**
	 * 
	 * @return
	 * 			a list with all known users owning a copy of this file
	 */
	public Vector<UserInfo> getOwningUsers(){
		return owningUsers;
	}
	
	/**
	 * adds a user to the list of users owning the file
	 * 
	 * @param newUser
	 * 			the user to add to the list
	 */
	public void addOwningUser(UserInfo newUser){
		owningUsers.add(newUser);
	}
	
	/**
	 * removes a user from the list of users owning the file
	 * 
	 * @param user
	 * 			the user that should be deleted
	 * @return
	 * 			true if the list contained the user
	 */
	public boolean removeOwningUser(UserInfo user){
		return owningUsers.remove(user);
	}
	
}
