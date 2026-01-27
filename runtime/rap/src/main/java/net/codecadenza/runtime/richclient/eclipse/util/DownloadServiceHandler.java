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
package net.codecadenza.runtime.richclient.eclipse.util;

import jakarta.servlet.ServletException;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import org.eclipse.rap.rwt.RWT;
import org.eclipse.rap.rwt.service.ServiceHandler;
import org.eclipse.rap.rwt.service.ServiceManager;

/**
 * <p>
 * Global handler for download operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DownloadServiceHandler implements ServiceHandler {
	public static final String DOWNLOAD_HANDLER_NAME = "downloadServiceHandler";
	private static final int BUFFER_SIZE = 1024 * 8;

	static {
		final ServiceManager manager = RWT.getServiceManager();

		final var handler = new DownloadServiceHandler();
		manager.registerServiceHandler(DOWNLOAD_HANDLER_NAME, handler);
	}

	/**
	 * @param pathToFile
	 * @return the download URL
	 */
	public static String createURL(String pathToFile) {
		final var url = new StringBuilder();
		final String pathSeparator = FileSystems.getDefault().getSeparator();
		final String fileName = pathToFile.substring(pathToFile.lastIndexOf(pathSeparator) + 1);
		final String path = pathToFile.substring(0, pathToFile.lastIndexOf(pathSeparator) + 1);

		url.append(RWT.getServiceManager().getServiceHandlerUrl(DOWNLOAD_HANDLER_NAME));
		url.append('&').append("filename").append('=').append(fileName);
		url.append('&').append("path").append('=').append(path);

		return RWT.getResponse().encodeURL(url.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.rap.rwt.service.ServiceHandler#service(jakarta.servlet.http.HttpServletRequest,
	 * jakarta.servlet.http.HttpServletResponse)
	 */
	@Override
	public void service(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
		// Extract the fully qualified path name of the file to download
		final String fileName = request.getParameter("filename");
		final String path = request.getParameter("path");
		final File file = Path.of(path, fileName).toFile();

		// Check if the file exists
		if (!file.exists() || !file.isFile()) {
			response.sendError(HttpServletResponse.SC_NOT_FOUND, "File '" + fileName + "' could not be found");
			return;
		}

		// Stream the content of the file
		response.setContentType("application/octet-stream");
		response.setHeader("Content-Length", Long.toString(file.length()));
		response.setHeader("Content-Disposition", "attachment; filename=\"" + fileName + "\"");

		try (final var fileInputStream = new FileInputStream(file);
				final ServletOutputStream outputStream = response.getOutputStream()) {
			final var buffer = new byte[BUFFER_SIZE];
			int bytesRead;

			while ((bytesRead = fileInputStream.read(buffer)) != -1) {
				outputStream.write(buffer, 0, bytesRead);
			}
		}
	}

}
