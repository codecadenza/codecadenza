/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.runtime.richclient.transport;

/**
 * <p>
 * Objects of this class are used to initialize a service locator
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ServiceLocatorDTO {
	private String alias;
	private String userName = "";
	private String password = "";
	private final String connectionURL;
	private final boolean encrypt;
	private boolean chunkedStreamingMode = true;
	private final TransportType transportType;

	/**
	 * Constructor
	 * @param alias the connection alias
	 * @param connectionURL the connection URL
	 * @param encrypt a flag that indicates if protocol should encrypt data transport
	 * @param transportType
	 */
	public ServiceLocatorDTO(String alias, String connectionURL, boolean encrypt, TransportType transportType) {
		this.alias = alias;
		this.connectionURL = connectionURL;
		this.encrypt = encrypt;
		this.transportType = transportType;
	}

	/**
	 * @return the encryption flag
	 */
	public boolean encrypt() {
		return encrypt;
	}

	/**
	 * @return the connection URL
	 */
	public String getConnectionURL() {
		return connectionURL;
	}

	/**
	 * @return the password
	 */
	public String getPassword() {
		return password;
	}

	/**
	 * @return the user name
	 */
	public String getUserName() {
		return userName;
	}

	/**
	 * @return the connection alias
	 */
	public String getAlias() {
		return alias;
	}

	/**
	 * @param alias
	 */
	public void setAlias(String alias) {
		this.alias = alias;
	}

	/**
	 * @param userName
	 */
	public void setUserName(String userName) {
		this.userName = userName;
	}

	/**
	 * @param password
	 */
	public void setPassword(String password) {
		this.password = password;
	}

	/**
	 * @return true if the data transport should be encrypted
	 */
	public boolean isEncrypt() {
		return encrypt;
	}

	/**
	 * @param chunkedStreamingMode
	 */
	public void setChunkedStreamingMode(boolean chunkedStreamingMode) {
		this.chunkedStreamingMode = chunkedStreamingMode;
	}

	/**
	 * @return true if file upload and download operations should stream a HTTP request body without internal buffering
	 */
	public boolean isChunkedStreamingMode() {
		return chunkedStreamingMode;
	}

	/**
	 * @return the transport type
	 */
	public TransportType getTransportType() {
		return transportType;
	}

}
