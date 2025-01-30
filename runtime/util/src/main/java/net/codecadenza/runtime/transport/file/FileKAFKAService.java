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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import net.codecadenza.runtime.transport.RemoteOperationException;

/**
 * <p>
 * Service interface for file operations via Kafka
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface FileKAFKAService {
	/**
	 * Download a file
	 * @param pathOnServer the fully qualified path of the file on the remote system
	 * @param targetFile the local target file
	 * @throws IOException if the download operation has failed
	 */
	void downloadFile(String pathOnServer, File targetFile) throws IOException;

	/**
	 * Upload a file
	 * @param file the local file to be uploaded
	 * @return the fully qualified path of the file created on the server
	 * @throws RemoteOperationException if the upload operation has failed
	 * @throws FileNotFoundException if the specified local file could not be found
	 */
	String uploadFile(File file) throws FileNotFoundException;

}
