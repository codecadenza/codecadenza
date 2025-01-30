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
package net.codecadenza.runtime.webclient.vaadin.component;

import com.vaadin.flow.component.DetachEvent;
import com.vaadin.flow.component.html.Anchor;
import com.vaadin.flow.function.SerializableSupplier;
import com.vaadin.flow.server.RequestHandler;
import com.vaadin.flow.server.VaadinRequest;
import com.vaadin.flow.server.VaadinResponse;
import com.vaadin.flow.server.VaadinSession;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.UUID;

/**
 * <p>
 * Anchor for dynamic file downloads
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FileDownloadAnchor extends Anchor {
	private static final long serialVersionUID = -4128499914018297046L;

	private final String handlerId = UUID.randomUUID().toString();
	private final RequestHandler requestHandler;

	/**
	 * Constructor
	 * @param fileHandler the handler that provides the file that should be downloaded
	 */
	public FileDownloadAnchor(SerializableSupplier<File> fileHandler) {
		this.requestHandler = new DownloadRequestHandler(fileHandler, handlerId);

		getElement().setAttribute("download", true);

		getElement().getNode().runWhenAttached(ui -> ui.beforeClientResponse(this, execution -> {
			ui.getSession().addRequestHandler(this.requestHandler);

			setHref("./" + handlerId);
		}));
	}

	/*
	 * Request handler that invokes the file handler and prepares the response in order to download the file
	 */
	private static class DownloadRequestHandler implements RequestHandler {
		private static final long serialVersionUID = -6022163832072018769L;

		private final SerializableSupplier<File> fileHandler;
		private final String handlerId;

		/**
		 * Constructor
		 * @param fileHandler
		 * @param handlerId
		 */
		public DownloadRequestHandler(SerializableSupplier<File> fileHandler, String handlerId) {
			this.fileHandler = fileHandler;
			this.handlerId = handlerId;
		}

		/*
		 * (non-Javadoc)
		 * @see com.vaadin.flow.server.RequestHandler#handleRequest(com.vaadin.flow.server.VaadinSession,
		 * com.vaadin.flow.server.VaadinRequest, com.vaadin.flow.server.VaadinResponse)
		 */
		@Override
		public boolean handleRequest(VaadinSession session, VaadinRequest request, VaadinResponse response) throws IOException {
			if (!request.getPathInfo().endsWith(handlerId))
				return false;

			// Get the actual file that should be downloaded
			final var file = fileHandler.get();

			if (file == null)
				return false;

			response.setStatus(200);
			response.setHeader("Content-Disposition", "attachment; filename=\"" + file.getName() + "\"");

			Files.copy(file.toPath(), response.getOutputStream());

			return true;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.Component#onDetach(com.vaadin.flow.component.DetachEvent)
	 */
	@Override
	protected void onDetach(DetachEvent detachEvent) {
		getUI().ifPresent(ui -> ui.getSession().removeRequestHandler(requestHandler));

		super.onDetach(detachEvent);
	}

}
