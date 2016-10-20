package studyproject.API.Lvl.Mid.Core;

import java.util.Vector;

import studyproject.API.Core.File.FileInfo;

/**
 * class to store information about a file from other clients in also contains a
 * list of clients that have this specific file on their disks
 * 
 * @author Michael
 *
 */
public class RemoteFileInfo {

	private String path;
	private long size;
	private String checksum;
	private Vector<UserInfo> owningUsers;

	/**
	 * //TODO this is the name of the file is it not? it does not have to have
	 * the same path on different clients
	 * 
	 * @param path
	 *            the name of the file
	 * 
	 * @param size
	 *            the size of the file
	 * 
	 * @param checksum
	 *            the checksum of the file
	 */
	public RemoteFileInfo(String path, long size, String checksum) {
		this.path = path;
		this.size = size;
		this.checksum = checksum;
		owningUsers = new Vector<UserInfo>();
	}

	/**
	 * creates a RemoteFileInfo object from the data obtained from the fileInfo
	 * object and adds the given user as first user to the list of owning users
	 * 
	 * @param fileInfo
	 *            the fileInfo from which to obtain the necessary information
	 * 
	 * @param user
	 *            the user to put into the list of owning users
	 */
	public RemoteFileInfo(FileInfo fileInfo, UserInfo user) {
		this(fileInfo.fileName, fileInfo.size, fileInfo.checksum);
		owningUsers.add(user);
	}

	/**
	 * creates a RemoteFileInfo object from the data obtained from the fileInfo
	 * object
	 * 
	 * @param fileInfo
	 *            the fileInfo from which to obtain the necessary information
	 * 
	 */
	public RemoteFileInfo(FileInfo fileInfo) {
		this(fileInfo.fileName, fileInfo.size, fileInfo.checksum);
	}

	/**
	 * 
	 * @return the name of the file
	 */
	public String getPath() {
		return path;
	}

	/**
	 * 
	 * @param path
	 *            the name of the file
	 */
	public void setPath(String path) {
		this.path = path;
	}

	/**
	 * 
	 * @return the size of the file
	 */
	public long getSize() {
		return size;
	}

	/**
	 * 
	 * @param size
	 *            the size of the file
	 */
	public void setSize(long size) {
		this.size = size;
	}

	/**
	 * 
	 * @return the checksum of the file
	 */
	public String getChecksum() {
		return checksum;
	}

	/**
	 * 
	 * @param checksum
	 *            the checksum of the file
	 */
	public void setChecksum(String checksum) {
		this.checksum = checksum;
	}

	/**
	 * 
	 * @return a list with all known users owning a copy of this file
	 */
	public Vector<UserInfo> getOwningUsers() {
		return owningUsers;
	}

	/**
	 * adds a user to the list of users owning the file
	 * 
	 * @param newUser
	 *            the user to add to the list
	 */
	public void addOwningUser(UserInfo newUser) {
		owningUsers.add(newUser);
	}

	/**
	 * removes a user from the list of users owning the file
	 * 
	 * @param user
	 *            the user that should be deleted
	 * @return true if the list contained the user
	 */
	public boolean removeOwningUser(UserInfo user) {
		return owningUsers.remove(user);
	}

	/**
	 * only returns true if the object is not null, is an object of this class,
	 * and the checksum, size and path are the same
	 */
	@Override
	public boolean equals(Object obj) {
		if (obj == null) {
			return false;
		} else if (!obj.getClass().equals(this.getClass())) {
			return false;
		}
		RemoteFileInfo fileInfo = (RemoteFileInfo) obj;
		if (!this.checksum.equals(fileInfo.getChecksum())) {
			return false;
		} else if (this.size != fileInfo.size) {
			return false;
		} else if (!this.path.equals(fileInfo.path)) {
			return false;
		}
		// } else if (!this.owningUsers.equals(fileInfo.owningUsers)) {
		// return false;
		// }
		return true;
	}

}
