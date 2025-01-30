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
package net.codecadenza.runtime.transport.file;

import net.codecadenza.runtime.transport.RemoteOperationException;

/**
 * <p>
 * Service interface for file operations via RMI
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface FileRMIService {
	/**
	 * Download a file
	 * @param pathOnServer the fully qualified path of the file on the remote system
	 * @return a byte array that represents the file content
	 * @throws RemoteOperationException if the download has failed
	 */
	byte[] downloadFile(String pathOnServer);

	/**
	 * Upload a file
	 * @param name the file name
	 * @param data a byte array that represents the file content
	 * @return the fully qualified path of the file created on the remote system
	 * @throws RemoteOperationException if the upload has failed
	 */
	String uploadFile(String name, byte[] data);

}
