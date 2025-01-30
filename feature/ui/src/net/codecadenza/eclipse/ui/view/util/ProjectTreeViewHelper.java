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
package net.codecadenza.eclipse.ui.view.util;

import java.util.HashMap;

/**
 * <p>
 * Helper class to support lazy loading of tree view items
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ProjectTreeViewHelper {
	private static HashMap<Integer, Boolean> projectMap = new HashMap<>();
	private static HashMap<Integer, Boolean> dbMap = new HashMap<>();
	private static HashMap<Integer, Boolean> dbTableMap = new HashMap<>();
	private static HashMap<String, Boolean> namespaceMap = new HashMap<>();
	private static HashMap<Integer, Boolean> javaTypeMap = new HashMap<>();

	/**
	 * Prevent instantiation
	 */
	private ProjectTreeViewHelper() {

	}

	/**
	 * Reset the helper in order to support a clean tree refresh
	 */
	public static void reset() {
		projectMap.clear();
		dbMap.clear();
		dbTableMap.clear();
		namespaceMap.clear();
		javaTypeMap.clear();
	}

	/**
	 * Add a project
	 * @param projectId the id of the project
	 */
	public static void addProject(int projectId) {
		projectMap.put(projectId, false);
	}

	/**
	 * Check if the project is already loaded
	 * @param projectId the id of the project
	 * @return true if the project data is already loaded
	 */
	public static boolean isProjectLoaded(int projectId) {
		final Boolean result = projectMap.get(projectId);

		if (result == null)
			return false;

		return result;
	}

	/**
	 * Set a flag in order to indicate that a project is fully loaded
	 * @param projectId the id of the project
	 */
	public static void setProjectLoaded(int projectId) {
		projectMap.put(projectId, true);
	}

	/**
	 * Add a namespace
	 * @param nameSpacePath
	 */
	public static void addNamespace(String nameSpacePath) {
		namespaceMap.put(nameSpacePath, false);
	}

	/**
	 * Check if the namespace is already loaded
	 * @param nameSpacePath
	 * @return true if the namespace is already loaded
	 */
	public static boolean isNamespaceLoaded(String nameSpacePath) {
		final Boolean result = namespaceMap.get(nameSpacePath);

		if (result == null)
			return false;

		return result;
	}

	/**
	 * Set a flag in order to indicate that a namespace is fully loaded
	 * @param nameSpacePath
	 */
	public static void setNamespaceLoaded(String nameSpacePath) {
		namespaceMap.put(nameSpacePath, true);
	}

	/**
	 * Add a Java type
	 * @param javaTypeHashCode the hash code of the Java type
	 */
	public static void addJavaType(int javaTypeHashCode) {
		javaTypeMap.put(javaTypeHashCode, false);
	}

	/**
	 * Check if the Java type is already loaded
	 * @param javaTypeHashCode the hash code of the Java type
	 * @return true if the Java type data is already loaded
	 */
	public static boolean isJavaTypeLoaded(int javaTypeHashCode) {
		final Boolean result = javaTypeMap.get(javaTypeHashCode);

		if (result == null)
			return false;

		return result;
	}

	/**
	 * Set a flag in order to indicate that a Java type is fully loaded
	 * @param javaTypeHashCode the hash code of the Java type
	 */
	public static void setJavaTypeLoaded(int javaTypeHashCode) {
		javaTypeMap.put(javaTypeHashCode, true);
	}

	/**
	 * Add a database
	 * @param dbId the id of the database
	 */
	public static void addDatabase(int dbId) {
		dbMap.put(dbId, false);
	}

	/**
	 * Check if the database is already loaded
	 * @param dbId the id of the database
	 * @return true if the database is already loaded
	 */
	public static boolean isDBLoaded(int dbId) {
		final Boolean result = dbMap.get(dbId);

		if (result == null)
			return false;

		return result;
	}

	/**
	 * Set a flag in order to indicate that a database is fully loaded
	 * @param dbId the id of the database
	 */
	public static void setDBLoaded(int dbId) {
		dbMap.put(dbId, true);
	}

	/**
	 * Add a database table
	 * @param tableId the id of the table
	 */
	public static void addDatabaseTable(int tableId) {
		dbTableMap.put(tableId, false);
	}

	/**
	 * Check if a database table is already loaded
	 * @param tableId the id of the database table
	 * @return true if the database table is already loaded
	 */
	public static boolean isTableLoaded(int tableId) {
		final Boolean result = dbTableMap.get(tableId);

		if (result == null)
			return false;

		return result;
	}

	/**
	 * Set a flag in order to indicate that a database table is fully loaded
	 * @param tableId the id of the database table
	 */
	public static void setTableLoaded(int tableId) {
		dbTableMap.put(tableId, true);
	}

}
