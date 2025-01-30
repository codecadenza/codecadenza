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
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import net.codecadenza.runtime.property.PropertyService;
import net.codecadenza.runtime.stream.StreamWorker;
import net.codecadenza.runtime.transport.TransportConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Servlet for generic file upload
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@WebServlet(value = "/upload", name = "FileUploadServlet")
public class FileUploadServlet extends HttpServlet {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = -8792320718931743029L;
	private static final String FILE_EXCHANGE_FOLDER = new PropertyService()
			.getStringProperty(PropertyService.PROP_EXCHANGE_FOLDER);
	private static final String SECRET = new PropertyService().getStringProperty(PropertyService.PROP_TRANSPORT_SECRET);

	/*
	 * (non-Javadoc)
	 * @see jakarta.servlet.http.HttpServlet#doPost(jakarta.servlet.http.HttpServletRequest,
	 * jakarta.servlet.http.HttpServletResponse)
	 */
	@Override
	public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		var fileName = "";
		StreamWorker streamWorker;
		File tempFile;

		try {
			streamWorker = new StreamWorker(SECRET);

			// Check if the data must be encrypted
			final String propertyEncrypt = request.getHeader(TransportConstants.REQ_PROP_ENCRYPTION);
			final Boolean encrypted = Boolean.valueOf(propertyEncrypt);

			fileName = request.getHeader(TransportConstants.REQ_PROP_FILE_NAME);

			logger.debug("Upload file '{}'", fileName);

			if (!FILE_EXCHANGE_FOLDER.isEmpty()) {
				// In case of clustered applications it is necessary to have one common place where all files are saved! Otherwise it is
				// very likely that a file is saved physically on one server and a follow-up request that is handled by another server is
				// not able to find that file!
				tempFile = new File(FILE_EXCHANGE_FOLDER + fileName + System.currentTimeMillis());

				if (!tempFile.createNewFile())
					throw new IllegalStateException("The file " + tempFile.getName() + " already exists!");
			}
			else {
				// Save the file at a temporary location
				tempFile = File.createTempFile(fileName, Long.toString(System.currentTimeMillis()));
			}

			try (final var fout = new FileOutputStream(tempFile)) {
				// Read the data from the input stream and write it to the file
				streamWorker.writeToOutput(request.getInputStream(), fout, false, encrypted);

				// Send the fully qualified path of the file back to the client
				streamWorker.writeObjectToStream(tempFile.getAbsolutePath(), response.getOutputStream(), encrypted);
			}
		}
		catch (final Exception e) {
			logger.error("Error while performing file upload operation!", e);

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
