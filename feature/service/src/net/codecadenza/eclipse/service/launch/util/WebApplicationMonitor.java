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

import java.net.HttpURLConnection;
import java.net.URL;
import java.time.Duration;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import org.eclipse.debug.core.ILaunch;

/**
 * <p>
 * Utility class that monitors whether the URL of a launched web application is available
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class WebApplicationMonitor {
	private static final int CONNECTION_TIMEOUT_MILLIS = 50;
	private static final int URL_CHECK_INTERVAL_MILLIS = 500;
	private static final int TERMINATION_CHECK_INTERVAL_MILLIS = 100;

	private final ScheduledExecutorService httpCheckScheduler = Executors.newSingleThreadScheduledExecutor();
	private final ScheduledExecutorService terminationScheduler = Executors.newSingleThreadScheduledExecutor();
	private final URL url;
	private final List<ILaunch> launchedApplications;
	private final Duration timeOut;

	/**
	 * Constructor
	 * @param url the URL that should be monitored
	 * @param launchedApplications a list of launched applications to check if at least one has been terminated
	 * @param timeOut the duration to wait for the web application to be ready
	 */
	public WebApplicationMonitor(URL url, List<ILaunch> launchedApplications, Duration timeOut) {
		this.url = url;
		this.launchedApplications = launchedApplications;
		this.timeOut = timeOut;
	}

	/**
	 * Start a background job that periodically sends a request to the given URL. If the HTTP status code is OK (200) or if the
	 * respective application has been terminated the task will be suspended and the {@link WebApplicationLaunchStatus} will no
	 * longer block!
	 * @return {@link WebApplicationLaunchStatus} that blocks until the requested resource is available
	 */
	public WebApplicationLaunchStatus start() {
		final var launchStatus = new WebApplicationLaunchStatus();
		final var stopChecking = new AtomicBoolean();
		final var shutdown = new AtomicBoolean();

		// Start a background task that checks if the requested resource is available
		httpCheckScheduler.scheduleAtFixedRate(() -> {
			stopChecking.set(isHttpResourceAvailable());

			if (stopChecking.get()) {
				launchStatus.countDown();
				httpCheckScheduler.shutdownNow();
			}
		}, 0, URL_CHECK_INTERVAL_MILLIS, TimeUnit.MILLISECONDS);

		// Start a background task that checks if at least one of the launched applications has been terminated
		terminationScheduler.scheduleAtFixedRate(() -> {
			final var launchTerminated = launchedApplications.stream().anyMatch(ILaunch::isTerminated);

			if (launchTerminated || shutdown.get()) {
				launchStatus.setTerminated();
				stopChecking.set(true);
			}

			if (stopChecking.get())
				terminationScheduler.shutdownNow();
		}, 0, TERMINATION_CHECK_INTERVAL_MILLIS, TimeUnit.MILLISECONDS);

		// After some time, stop all other tasks, as there is no point in waiting forever if the URL cannot be reached!
		final var shutdownTimer = new Timer();
		shutdownTimer.schedule(new TimerTask() {
			/*
			 * (non-Javadoc)
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public void run() {
				shutdown.set(true);
			}
		}, timeOut.toMillis());

		return launchStatus;
	}

	/**
	 * Check if the given URL is available
	 * @return true if the given URL is available
	 */
	private boolean isHttpResourceAvailable() {
		try {
			final HttpURLConnection connection = (HttpURLConnection) url.openConnection();
			connection.setConnectTimeout(CONNECTION_TIMEOUT_MILLIS);
			connection.connect();

			return connection.getResponseCode() == HttpURLConnection.HTTP_OK;
		}
		catch (final Exception _) {
			// Ignore all exceptions here!
		}

		return false;
	}

}
