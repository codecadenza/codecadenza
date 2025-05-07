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
package net.codecadenza.eclipse.service.launch.util;

import java.awt.Desktop;
import java.net.URL;
import net.codecadenza.eclipse.service.CodeCadenzaServicePlugin;
import net.codecadenza.eclipse.tools.util.os.OperatingSystem;

/**
 * <p>
 * Background task that waits for the given {@link WebApplicationLaunchStatus} and opens the URL of a web application in the
 * default browser
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class WebApplicationLauncher extends Thread {
	private static final String OPEN_COMMAND = "open";

	private final WebApplicationLaunchStatus status;
	private final URL url;

	/**
	 * Constructor
	 * @param url
	 * @param status
	 */
	public WebApplicationLauncher(URL url, WebApplicationLaunchStatus status) {
		this.status = status;
		this.url = url;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Thread#run()
	 */
	@Override
	public void run() {
		// Wait until the web application is ready!
		if (!status.isReady())
			return;

		try {
			if (OperatingSystem.isMacOS() || OperatingSystem.isWindows())
				Desktop.getDesktop().browse(url.toURI());
			else
				Runtime.getRuntime().exec(new String[] { OPEN_COMMAND, url.toString() });
		}
		catch (final Exception e) {
			CodeCadenzaServicePlugin.getDefault().getLog().error("Error while opening the URL '" + url.toString() + "' in the browser!",
					e);
		}
	}

}
