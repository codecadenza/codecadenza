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
package net.codecadenza.runtime.ddt.service.data;

import java.time.Duration;
import java.util.List;
import java.util.UUID;
import net.codecadenza.runtime.ddt.model.MethodInvocation;
import net.codecadenza.runtime.ddt.model.TestData;
import net.codecadenza.runtime.ddt.service.data.util.MethodInvocationGroupIterator;

/**
 * <p>
 * An interface that must be implemented by all test data providers
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public interface ITestDataProvider {
	/**
	 * Load the test data
	 * @return the test data
	 */
	TestData loadTestData();

	/**
	 * Get the next {@link MethodInvocation}
	 * @return the method invocation
	 */
	MethodInvocation getNextInvocation();

	/**
	 * Put the supplied {@link MethodInvocation} back to the front of the internal queue
	 * @param methodInvocation the invocation that must be re‑queued
	 */
	void pushBackInvocation(MethodInvocation methodInvocation);

	/**
	 * Get the next method parameter
	 * @param <T> the requested return type
	 * @param type the {@link Class} of the requested type
	 * @return the parameter
	 */
	<T> T getNextParameter(Class<T> type);

	/**
	 * Get the next method parameter as a list
	 * @param <T> the requested return type
	 * @param type the {@link Class} of the requested type
	 * @return a list of test objects for the given parameter
	 */
	<T> List<T> getNextListParameter(Class<T> type);

	/**
	 * Get the expected return value
	 * @param <T> the requested return type
	 * @param type the {@link Class} of the requested type
	 * @return the return value
	 */
	<T> T getReturnValue(Class<T> type);

	/**
	 * Get the return value as a list
	 * @param <T> the requested return type
	 * @param type the {@link Class} of the requested type
	 * @return a list of objects
	 */
	<T> List<T> getReturnListValue(Class<T> type);

	/**
	 * Set the value of the field with the given ID
	 * @param <T> the type of the value to set
	 * @param id the ID of the field
	 * @param value the value to set. Note, that only standard types (e.g. int, Double, String etc.) are supported!
	 * @param type the {@link Class} of the type
	 */
	<T> void setGeneratedFieldValue(UUID id, T value, Class<T> type);

	/**
	 * Get the expected number of objects returned by a given field if it is mapped to a list
	 * @param id the ID of the field
	 * @return the expected number of objects
	 */
	Integer getExpectedSizeOfField(UUID id);

	/**
	 * @return the post-processing statement of the current method invocation
	 */
	String getPostProcessingStatement();

	/**
	 * @return the timeout of the current method invocation
	 */
	Duration getTimeout();

	/**
	 * @return the group ID of the current method invocation
	 */
	UUID getGroupId();

	/**
	 * @return the iterator for all method invocations of the same group
	 */
	MethodInvocationGroupIterator getMethodInvocationGroupIterator();

	/**
	 * @return the expected number of elements in the list returned by the current method invocation
	 */
	Integer getExpectedSize();

}
