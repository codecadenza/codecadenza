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
package net.codecadenza.eclipse.service.launch.config;

import java.time.Duration;
import org.eclipse.debug.core.ILaunchConfiguration;

/**
 * <p>
 * Record that contains a launch configuration and additional data for opening a web application in the browser
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @param launchConfiguration the launch configuration
 * @param url the URL to be opened in the browser
 * @param timeOut the maximum time to wait until a web application is ready
 * @param isDebugSupported flag that controls if the application can be started in debug mode
 * @author Martin Ganserer
 * @version 1.0.0
 */
public record LaunchSettings(ILaunchConfiguration launchConfiguration, String url, Duration timeOut, boolean isDebugSupported) {

}
