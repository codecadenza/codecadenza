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
package net.codecadenza.eclipse.testing.domain;

/**
 * <p>
 * Enumeration of supported integration bean types
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public enum IntegrationBeanType {
	KAFKA, REST, RMI, SOAP, JMS;

	/**
	 * Get the {@link IntegrationBeanType} from the provided package name
	 * @param packageName
	 * @return the {@link IntegrationBeanType}
	 * @throws IllegalArgumentException if the {@link IntegrationBeanType} could not be created from the provided package name
	 */
	public static IntegrationBeanType fromPackageName(String packageName) {
		final int lastPackageDelimiterPos = packageName.lastIndexOf('.');

		if (lastPackageDelimiterPos == -1)
			throw new IllegalArgumentException("The package name '" + packageName + "' is invalid!");

		return IntegrationBeanType.valueOf(packageName.substring(lastPackageDelimiterPos + 1).toUpperCase());
	}

}
