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
package net.codecadenza.runtime.reflect;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

/**
 * <p>
 * Utility class for finding a method by its name and its parameters
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MethodFinder {
	/**
	 * Prevent instantiation
	 */
	private MethodFinder() {

	}

	/**
	 * Find the method
	 * @param cl the class
	 * @param methodName the name of the method
	 * @param params the parameters
	 * @return the method
	 * @throws NoSuchMethodException if the method could not be found
	 */
	public static Method findMethod(Class<?> cl, String methodName, Object[] params) throws NoSuchMethodException {
		final var methodMap = new HashMap<String, List<Method>>();
		final var paramMap = new HashMap<Method, Object[]>();

		// Get all methods of the class
		final Method[] methods = cl.getMethods();

		// Fill an internal hash map with all class methods
		Arrays.asList(methods).forEach(method -> {
			final String mName = method.getName();

			List<Method> methodList = methodMap.get(mName);

			if (methodList == null) {
				methodList = new ArrayList<>();
				methodMap.put(mName, methodList);
			}

			methodList.add(method);

			final Class<?>[] paramTypes = method.getParameterTypes();
			paramMap.put(method, paramTypes);
		});

		final List<Method> methodList = methodMap.get(methodName);

		if (methodList == null)
			throw new NoSuchMethodException(methodName);

		// Find the method
		for (final Method m : methodList) {
			boolean ok = true;
			final var paramTypes = (Class[]) paramMap.get(m);

			if (params == null)
				params = new Object[0];

			if (paramTypes.length != params.length)
				continue;

			for (int j = 0; j < paramTypes.length; j++) {
				if (params[j] == null)
					continue;

				final Class<?> c = params[j].getClass();
				final Class<?> paramClass = paramTypes[j];

				if (paramClass.isPrimitive()) {
					if (paramClass.equals(Integer.TYPE)) {
						if (c.equals(Integer.class))
							continue;

						ok = false;
					}
					else if (paramClass.equals(Double.TYPE)) {
						if (c.equals(Double.class))
							continue;

						ok = false;
					}
					else if (paramClass.equals(Boolean.TYPE)) {
						if (c.equals(Boolean.class))
							continue;

						ok = false;
					}
					else if (paramClass.equals(Long.TYPE)) {
						if (c.equals(Long.class))
							continue;

						ok = false;
					}
					else if (paramClass.equals(Character.TYPE)) {
						if (c.equals(Character.class))
							continue;

						ok = false;
					}
					else if (paramClass.equals(Byte.TYPE)) {
						if (c.equals(Byte.class))
							continue;

						ok = false;
					}
					else if (paramClass.equals(Float.TYPE)) {
						if (c.equals(Float.class))
							continue;

						ok = false;
					}
					else if (paramClass.equals(Short.TYPE)) {
						if (c.equals(Short.class))
							continue;

						ok = false;
					}

					break;
				}

				if (!paramClass.isAssignableFrom(c)) {
					ok = false;
					break;
				}
			}

			if (ok)
				return m;
		}

		throw new NoSuchMethodException(methodName);
	}

}
