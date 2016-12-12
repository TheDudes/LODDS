package studyproject.API.Lvl.Low;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.util.logging.Level;

import studyproject.API.Errors.ErrLog;
import studyproject.logging.APILvl;
import studyproject.logging.LogKey;

/**
 * Class that sends requests to other clients,
 * formatted according to the LODDS specification
 * @author Michael
 *
 */
public class Requests {

	private static final String GET_INFO_UP = "get info ";
	private static final String GET_FILE = "get file ";
	private static final String GET_SEND_PERMISSION = "get send-permission ";

	/**
	 * Requests an updated fileInfo list from the other client, the timestamp is
	 * the time of last synchronization. If the timestamp is 0 the command is a
	 * get_all command
	 * 
	 * @param socketStream
	 *            the stream to write to
	 * 
	 * @param timestamp
	 *            time of last synchronization, if 0 the client has to send a
	 *            full list of all files it has
	 * 
	 * @return 0 or an error value
	 */
	public static int getInfo(BufferedOutputStream socketStream, long timestamp) {
		try {
			socketStream.write((GET_INFO_UP + timestamp + "\n").getBytes());
			socketStream.flush();
		} catch (IOException e) {
			ErrLog.log(Level.SEVERE, LogKey.error, APILvl.low, "getInfo",
					"IOException thrown: " + e.getStackTrace());
			return 1;
		}
		return 0;
	}

	/**
	 * Requests the transfer of a file from the other client. Can also request
	 * only parts of a file by setting the start and/or endIndex to something
	 * else than 0 or the size of the file
	 * 
	 * @param socketStream
	 *            the stream to write the request to
	 * 
	 * @param checksum
	 *            the checksum of the requested file
	 * 
	 * @param startIndex
	 *            the index from which the other client should start reading the
	 *            file
	 * 
	 * @param endIndex
	 *            the index at which the other client should stop reading the
	 *            file
	 * 
	 * @return 0 or an error value
	 */
	public static int getFile(BufferedOutputStream socketStream, String checksum, long startIndex, long endIndex) {
		try {
			socketStream.write((GET_FILE + checksum + " " + startIndex + " " + endIndex + "\n").getBytes());
			socketStream.flush();
		} catch (IOException e) {
			ErrLog.log(Level.SEVERE, LogKey.error, APILvl.low, "getInfo",
					"IOException thrown: " + e.getStackTrace());
			return 1;
		}
		return 0;
	}

	/**
	 * Requests the send permission for a file by writing the arguments to the
	 * stream in the way defined in the specification
	 * 
	 * @param socketStream
	 *            the stream to write to
	 * 
	 * @param size
	 *            the size of the file that permission to send is asked for
	 * 
	 * @param timeout
	 *            the time until the request times out. Not a timestamp but a
	 *            set amount of time in ms
	 * 
	 * @param fileName
	 *            the name of the file that permission to send is asked for
	 * 
	 * @return 0 or an error value
	 */
	public static int getSendPermission(BufferedOutputStream socketStream, long size, long timeout, String fileName) {
		try {
			socketStream.write((GET_SEND_PERMISSION + size + " " + timeout + " " + fileName + "\n").getBytes());
			socketStream.flush();
		} catch (IOException e) {
			ErrLog.log(Level.SEVERE, LogKey.error, APILvl.low, "getInfo",
					"IOException thrown: " + e.getStackTrace());
			return 1;
		}
		return 0;
	}

}
