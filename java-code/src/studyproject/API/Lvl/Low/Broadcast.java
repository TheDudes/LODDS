package studyproject.API.Lvl.Low;

import java.io.IOException;
import java.net.*;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.NoSuchElementException;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import studyproject.API.Core.BroadcastInfo;
import studyproject.API.Core.Utils;
import studyproject.API.Errors.ErrorFactory;
import studyproject.logging.LogKey;

/**
 * Class can send an receive broadcasts that are in accordance with the LODDS
 * specification Can also display all currently active network devices of the
 * local machine
 * 
 * @author Michael
 *
 */
public class Broadcast {
	//TODO Where to set the user chosen ports if they are available
	private static final int DEFAULT_PORT = 9002;
	private static final int DEFAULT_PORT_SENDING = 9003;
	private static final int DEFAULT_BUFFERSIZE = 2048;
	private static final String ADVERTISE_BROADCAST_REGEX = "\\S*@(\\d{1,3}\\.){3}\\d{1,3}:\\d{1,5} \\d{1,19} \\d{1,19}";// "(\\d{1,3}\\.){3}\\d{1,3}
																															// \\d{1,5}
																															// \\d{1,19}
																															// \\d{1,19}
																															// \\S*";
	private static final String IP_REGEX = "(\\d{1,3}\\.){3}\\d{1,3}";

	private static int broadcastPort = DEFAULT_PORT;
	private static int outgoingPort = DEFAULT_PORT_SENDING;
	private static int buffersize = DEFAULT_BUFFERSIZE;

	/**
	 * retrieves a list with all network devices the local machine has that are
	 * running and not virtual
	 * 
	 * @param networkAddresses
	 *            the list in which the addresses are put into
	 * 
	 * @return 0 or an error code
	 */
	public static int getNetworkAddresses(ArrayList<String> networkAddresses) {
		try {
			Enumeration<NetworkInterface> networkInterfaces = NetworkInterface.getNetworkInterfaces();
			while (networkInterfaces.hasMoreElements()) {
				NetworkInterface networkInterface = networkInterfaces.nextElement();
				if (networkInterface.isUp() && !networkInterface.isVirtual()) {
					networkAddresses.add(networkInterface.getDisplayName());
				}
			}
		} catch (SocketException e) {
			return 1;
		}
		return 0;
	}

	/**
	 * Retrieves the local broadcastAddress, needs an interface for getting the
	 * local ip
	 * 
	 * @param interfaceName
	 *            the name of the interface to which the ip should be retrieved
	 * 
	 * @param broadcastAddress
	 *            the StringBuilder to store the broadcastAddress in
	 * 
	 * @return 0 or an error code
	 */
	public static int getBroadcastAddress(String interfaceName, StringBuilder broadcastAddress) {
		int toReturn = getAddress(interfaceName, broadcastAddress);
		if (toReturn == 0) {
			broadcastAddress.delete(broadcastAddress.lastIndexOf(".") + 1, broadcastAddress.length());
			broadcastAddress.append("255");
		}
		return toReturn;
	}

	/**
	 * Retrieves the ip address of the given interface
	 * 
	 * @param interfaceName
	 *            the name of the interface to which the ip should be retrieved
	 * 
	 * @param networkAddress
	 *            the StringBuilder to store the ip in
	 * 
	 * @return 0 or an error code
	 */
	public static int getLocalIp(String interfaceName, StringBuilder networkAddress) {
		return getAddress(interfaceName, networkAddress);
	}

	private static int getAddress(String interfaceName, StringBuilder broadcastAddress) {
		try {
			Enumeration<NetworkInterface> networkInterfaces = NetworkInterface.getNetworkInterfaces();
			while (networkInterfaces.hasMoreElements()) {
				NetworkInterface networkInterface = networkInterfaces.nextElement();
				if (networkInterface.isUp() && !networkInterface.isVirtual()
						&& networkInterface.getDisplayName().equals(interfaceName)) {
					Enumeration<InetAddress> inetAddresses = networkInterface.getInetAddresses();
					while (inetAddresses.hasMoreElements()) {
						broadcastAddress.delete(0, broadcastAddress.length());
						broadcastAddress = broadcastAddress.append(inetAddresses.nextElement().getHostAddress());
						if (Pattern.matches(IP_REGEX, broadcastAddress)) {
							return 0;
						}
					}
				}
				broadcastAddress.delete(0, broadcastAddress.length());
			}
		} catch (SocketException e) {
			return 1;
		} catch (NoSuchElementException f) {
			Logger.getGlobal().log(ErrorFactory.build(Level.SEVERE, LogKey.error, f));
			System.exit(1);
		}
		Logger.getGlobal().log(ErrorFactory.build(Level.SEVERE, LogKey.error, "No Suitable Interface found"));
		System.exit(1);
		return 0;
	}

	/**
	 * sends out an UDP broadcast to all machines in the local network. This
	 * broadcast contains the network address of the current machine, the port
	 * other machines should contact it from, the timestamp of the last change
	 * in the tracked filesystem and the name under which this machine shall be
	 * displayed
	 * 
	 * @param broadcastAddress
	 *            the address the UDP socket has to broadcast to
	 * 
	 * @param networkAddress
	 *            the network address of this machine (TCP address)
	 * 
	 * @param ipPort
	 *            the port other machines should contact this machine from
	 * 
	 * @param timestamp
	 *            the time of the last change in the tracked filesystem of this
	 *            machine
	 * 
	 * @param load
	 *            how many bytes the client still has to send to other clients
	 *            as part of a file transfer
	 * 
	 * @param name
	 *            the name that other machines should display this machine as
	 * 
	 * @return 0 or an error code
	 */
	public static int sendAdvertise(String broadcastAddress, String networkAddress, int ipPort, long timestamp,
			long load, String name) {
		DatagramPacket packet;
		try (DatagramSocket socket = new DatagramSocket(outgoingPort, InetAddress.getByName(networkAddress))) {
			socket.setBroadcast(true);
			byte[] packetContents = (name + "@" + networkAddress + ":" + ipPort + " " + timestamp + " " + load + "\n")
					.getBytes();
			Logger.getGlobal().log(ErrorFactory.build(Level.INFO, LogKey.broadcastSent,
					networkAddress + " " + ipPort + " " + timestamp + " " + load + " " + name));
			packet = new DatagramPacket(packetContents, packetContents.length, InetAddress.getByName(broadcastAddress),
					broadcastPort);
			socket.send(packet);
			// socket.disconnect();
		} catch (UnknownHostException e) {
			Logger.getGlobal().log(ErrorFactory.build(Level.SEVERE, LogKey.broadcastSent, e));
			System.exit(1);
		} catch (SocketException e) {
			return 1;
		} catch (IOException e) {
			return 1;
		}
		return 0;
	}

	/**
	 * listens on given UPD address for the next UDP packet and then puts the
	 * given information in the broadcastInfo if the given data matches the
	 * specification
	 * 
	 *
	 * @param broadcastInfo
	 *            the broadcastInfo to put the read info in
	 * 
	 * @return 0 or an error value
	 */
	public static int readAdvertise(BroadcastInfo broadcastInfo) {
		try (DatagramSocket socket = new DatagramSocket(broadcastPort, InetAddress.getByName("0.0.0.0"))) {
			DatagramPacket packet = new DatagramPacket(new byte[buffersize], buffersize);
			socket.receive(packet);
			int index = 0;
			while (packet.getData()[index] != '\n') {
				index++;
				if (!(index < buffersize)) {
					return -2;
				}
			}
			String packetContents = new String(Utils.getBytesFromTo(packet.getData(), 0, index), "UTF-8");
			if (!Pattern.matches(ADVERTISE_BROADCAST_REGEX, packetContents)) {
				return 2;
			}
			String[] packetParts = packetContents.split(" |@|:");
			broadcastInfo.name = packetParts[0];
			broadcastInfo.networkAddress = packetParts[1];
			broadcastInfo.ipPort = Integer.parseInt(packetParts[2]);
			broadcastInfo.timestamp = Long.parseLong(packetParts[3]);
			broadcastInfo.load = Long.parseLong(packetParts[4]);
			Logger.getGlobal().log(ErrorFactory.build(Level.INFO, LogKey.broadcastReceived, broadcastInfo.toString()));

		} catch (IOException e) {
			return 1;
		} catch (NumberFormatException f) {
			return 2;
		}
		return 0;
	}

	/**
	 * 
	 * @return the port the UDP packets are sent to
	 */
	public static int getBroadcastPort() {
		return broadcastPort;
	}

	/**
	 * 
	 * @param broadcastPort
	 *            the port the UDP packets are sent to
	 */
	public static void setBroadcastPort(int broadcastPort) {
		Broadcast.broadcastPort = broadcastPort;
	}

	/**
	 * 
	 * @return the port the UDP socket is bound to locally
	 */
	public static int getOutgoingPort() {
		return outgoingPort;
	}

	/**
	 * 
	 * @param outgoingPort
	 *            the port the UDP socket is bound to locally
	 */
	public static void setOutgoingPort(int outgoingPort) {
		Broadcast.outgoingPort = outgoingPort;
	}

	/**
	 * 
	 * @return the size of the buffer used to read data from the UDP packet
	 */
	public static int getBuffersize() {
		return buffersize;
	}

	/**
	 * 
	 * @param buffersize
	 *            the size of the buffer used to read data from the UDP packet
	 */
	public static void setBuffersize(int buffersize) {
		Broadcast.buffersize = buffersize;
	}

}
