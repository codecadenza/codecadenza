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

import java.util.concurrent.CountDownLatch;

/**
 * <p>
 * Special {@link CountDownLatch} that tracks if a web application is ready to serve requests
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class WebApplicationLaunchStatus extends CountDownLatch {
	private boolean terminated;

	/**
	 * Constructor
	 */
	public WebApplicationLaunchStatus() {
		super(1);
	}

	/**
	 * Set the flag that indicates that the web application has been terminated
	 */
	public void setTerminated() {
		terminated = true;
	}

	/**
	 * @return true if the web application is ready
	 */
	public boolean isReady() {
		try {
			await();
		}
		catch (final InterruptedException _) {
			Thread.currentThread().interrupt();
			return false;
		}

		return !terminated;
	}

}
