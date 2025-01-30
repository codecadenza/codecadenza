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
package net.codecadenza.eclipse.ui.util.method;

import java.util.Collection;
import net.codecadenza.eclipse.model.java.JavaMethod;
import net.codecadenza.eclipse.model.java.MethodParameter;

/**
 * <p>
 * Utility class for creating methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MethodHelper {
	/**
	 * Prevent instantiation
	 */
	private MethodHelper() {

	}

	/**
	 * Generate a method identifier (e.g. methodName(paramType1, paramType2))
	 * @param m
	 * @return the identifier
	 */
	public static String generateMethodIdentifier(JavaMethod m) {
		final var b = new StringBuilder();
		b.append(m.getName() + "(");

		boolean isFirstParam = true;

		for (final MethodParameter p : m.getMethodParameters()) {
			if (isFirstParam) {
				if (p.getType() == null)
					return null;

				b.append(p.getType().getName());

				isFirstParam = false;
			}
			else {
				if (p.getType() == null)
					return null;

				b.append(", " + p.getType().getName());
			}
		}

		b.append(")");

		return b.toString();
	}

	/**
	 * Generate a method identifier (e.g. methodName(paramType1, paramType2))
	 * @param methodName
	 * @param parameters
	 * @return the identifier
	 */
	public static String generateMethodIdentifier(String methodName, Collection<MethodParameter> parameters) {
		final var b = new StringBuilder();
		b.append(methodName + "(");

		boolean isFirstParam = true;

		for (final MethodParameter p : parameters) {
			if (isFirstParam) {
				if (p.getType() == null)
					return null;

				b.append(p.getType().getName());

				isFirstParam = false;
			}
			else {
				if (p.getType() == null)
					return null;

				b.append(", " + p.getType().getName());
			}
		}

		b.append(")");

		return b.toString();
	}

}
