package studyproject.Test.Lvl.Mid;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Collections;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;

import org.junit.Test;

import studyproject.API.Loadbalancer.LoadComparator;
import studyproject.API.Loadbalancer.Loadbalancer;
import studyproject.API.Lvl.Mid.Core.FileCoreInfo;
import studyproject.API.Lvl.Mid.Core.UserInfo;

/**
 * class for junit tests for parts of the LoadBalancing classes
 * 
 * @author Michael
 *
 */
public class LoadBalancerTest {

	private String localhost = "127.0.0.1";
	private int port = 9002;
	private String testChecksum1 = "96fa8f226d3801741e807533552bc4b177ac4544d834073b6a5298934d34b40b";
	private String testFile1 = "testFile1";
	private String testChecksum2 = "1d98362d093cb271d4f03f89b4af93996b3bf43aae6294fef3bc4e4b61234f31";

	/**
	 * check if the correct user is returned when using the getClientMinLoad with
	 * a checksum that a user has
	 * 
	 * @throws UnknownHostException
	 */
	@Test
	public void testGetClientMinLoad() throws UnknownHostException {
		Vector<UserInfo> owningUsers = createUsers();
		Loadbalancer loadBalancer = new Loadbalancer(0, 0, null);
		UserInfo result = loadBalancer.getClientMinLoad(owningUsers, testChecksum1);
		assertEquals("junitTest", result.getUserName());
	}

	/**
	 * check if no user is returned when no user has the passed checksum
	 * in the getClientMinLoad
	 * 
	 * @throws UnknownHostException
	 */
	@Test
	public void testGetClientMinLoad2() throws UnknownHostException {
		Vector<UserInfo> owningUsers = createUsers();
		Loadbalancer loadBalancer = new Loadbalancer(0, 0, null);
		UserInfo result = loadBalancer.getClientMinLoad(owningUsers, testChecksum2);
		assertEquals(null, result);
	}

	/**
	 * checks if the Collections.sort method works with the written
	 * LoadComparator and on the userList
	 * 
	 * @throws UnknownHostException
	 */
	@Test
	public void testLoadComparator() throws UnknownHostException {
		LoadComparator loadComparator = new LoadComparator();
		Vector<UserInfo> owningUsers = createUsers();
		Collections.sort(owningUsers, loadComparator);
		System.out.println(owningUsers.get(0));
		System.out.println(owningUsers.size());
		assertEquals("junitTest", owningUsers.get(0).getUserName());
		assertTrue((owningUsers.get(0).getLoad() < owningUsers.get(1).getLoad())
				&& (owningUsers.get(1).getLoad()) < owningUsers.get(2).getLoad());
		
		owningUsers.get(0).setLoad(2000L);
		Collections.sort(owningUsers, loadComparator);
		assertEquals("testUser", owningUsers.get(0).getUserName());
		assertTrue((owningUsers.get(0).getLoad() < owningUsers.get(1).getLoad())
				&& (owningUsers.get(1).getLoad()) < owningUsers.get(2).getLoad());
	}

	/**
	 * Initialise a vector list with users
	 * 
	 * @return
	 * @throws UnknownHostException
	 */
	private Vector<UserInfo> createUsers() throws UnknownHostException {
		Vector<UserInfo> users = new Vector<>();
		Vector<String> fileNames = new Vector<String>();
		fileNames.add(testFile1);
		ConcurrentHashMap<String, FileCoreInfo> pathToFileInfo = new ConcurrentHashMap<String, FileCoreInfo>();
		ConcurrentHashMap<String, Vector<String>> checksumToPath = new ConcurrentHashMap<String, Vector<String>>();
		pathToFileInfo.put(testFile1, new FileCoreInfo(testChecksum1, 123123, testFile1));
		checksumToPath.put(testChecksum1, fileNames);
		UserInfo userInfo = new UserInfo(InetAddress.getByName(localhost), port, "testUser", 0L, 400L, pathToFileInfo,
				checksumToPath, 0L);
		users.add(userInfo);
		userInfo = new UserInfo(InetAddress.getByName(localhost), port, "junit", 0L, 903L, pathToFileInfo,
				checksumToPath, 0L);
		users.add(userInfo);
		userInfo = new UserInfo(InetAddress.getByName(localhost), port, "junitTest", 0L, 90L, pathToFileInfo,
				checksumToPath, 0L);
		users.add(userInfo);
		return users;
	}

}
