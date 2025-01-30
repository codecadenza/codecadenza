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
package net.codecadenza.runtime.server.transport;

import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import net.codecadenza.runtime.property.PropertyService;
import net.codecadenza.runtime.stream.StreamWorker;
import net.codecadenza.runtime.transport.TransportConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Servlet to download files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@WebServlet(value = "/download", name = "FileDownloadServlet")
public class FileDownloadServlet extends HttpServlet {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = -1100249810905720275L;
	private static final String SECRET = new PropertyService().getStringProperty(PropertyService.PROP_TRANSPORT_SECRET);

	/*
	 * (non-Javadoc)
	 * @see jakarta.servlet.http.HttpServlet#doPost(jakarta.servlet.http.HttpServletRequest,
	 * jakarta.servlet.http.HttpServletResponse)
	 */
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		String path = null;
		File downloadFile = null;
		StreamWorker streamWorker = null;

		// Check if the data is encrypted
		final String propertyEncrypt = request.getHeader(TransportConstants.REQ_PROP_ENCRYPTION);
		final Boolean encrypted = Boolean.valueOf(propertyEncrypt);

		logger.debug("Process file download request");

		try {
			streamWorker = new StreamWorker(SECRET);

			// First we check if the client sends proper data
			path = (String) streamWorker.readObjectFromStream(request.getInputStream(), encrypted);

			if (path == null) {
				logger.warn("Download operation failed. The request contained no path!");

				response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Unexpected content in request!");
				return;
			}

			// Check if the file exists!
			downloadFile = new File(path);

			if (!downloadFile.exists()) {
				logger.warn("Download operation failed. The file '{}' could not be found!", path);

				response.sendError(HttpServletResponse.SC_NOT_FOUND, "Requested file doesn't exist!");
				return;
			}

			logger.debug("Download file '{}'", path);

			response.setContentLength((int) downloadFile.length());

			// Write the file content to the output stream of the response object
			try (final var fin = new FileInputStream(downloadFile)) {
				streamWorker.writeToOutput(fin, response.getOutputStream(), encrypted, false);
			}
		}
		catch (final Exception e) {
			logger.error("Error while performing download operation!", e);

			sendError(response, e.getMessage());
		}
	}

	/**
	 * Send an error response back to the client. All exceptions will be suppressed!
	 * @param response
	 * @param message
	 */
	private void sendError(HttpServletResponse response, String message) {
		try {
			response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, message);
		}
		catch (final Exception e) {
			logger.error("Failed to send error code!", e);
		}
	}

}
