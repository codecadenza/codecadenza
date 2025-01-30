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
package net.codecadenza.eclipse.model.util;

/**
 * <p>
 * Helper class that generates getters and setters following the JavaBeans specification 1.01
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaBeanHelper {
	/**
	 * Prevent instantiation
	 */
	private JavaBeanHelper() {

	}

	/**
	 * @param name
	 * @return the converted name
	 */
	private static String convertName(String name) {
		String convertedName;

		if (name == null || name.isEmpty())
			return "";

		if (name.length() > 1) {
			if (Character.isUpperCase(name.charAt(1)))
				convertedName = name;
			else {
				final char[] chars = name.toCharArray();
				chars[0] = Character.toUpperCase(chars[0]);
				convertedName = new String(chars);
			}
		}
		else
			convertedName = name.toUpperCase();

		return convertedName;
	}

	/**
	 * @param name
	 * @return the property name
	 */
	public static String getPropertyName(String name) {
		if (name == null || name.isEmpty())
			return "";

		if (name.length() > 1 && Character.isUpperCase(name.charAt(1)) && Character.isUpperCase(name.charAt(0)))
			return name;

		if (Character.isUpperCase(name.charAt(0))) {
			final char[] chars = name.toCharArray();
			chars[0] = Character.toLowerCase(chars[0]);

			return new String(chars);
		}

		return name;
	}

	/**
	 * @param name
	 * @param isBoolean
	 * @return the name of the getter
	 */
	public static String getGetterName(String name, boolean isBoolean) {
		var prefix = "get";

		if (isBoolean)
			prefix = "is";

		return prefix + convertName(name) + "()";
	}

	/**
	 * @param name
	 * @return the name of the setter
	 */
	public static String getSetterName(String name) {
		return "set" + convertName(name);
	}

	/**
	 * @param name
	 * @param isBoolean
	 * @return the method reference of the getter
	 */
	public static String getGetterReference(String name, boolean isBoolean) {
		var prefix = "get";

		if (isBoolean)
			prefix = "is";

		return "::" + prefix + convertName(name);
	}

}
