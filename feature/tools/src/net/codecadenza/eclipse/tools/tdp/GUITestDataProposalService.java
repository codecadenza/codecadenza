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
package net.codecadenza.eclipse.tools.tdp;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.GUITestAction;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.GUITestData;
import net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin;
import net.codecadenza.eclipse.tools.util.db.DBManager;

/**
 * <p>
 * Service for providing test data proposals
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class GUITestDataProposalService {
	private static final int MAX_RESULTS = 10;
	private static final int LOOKUP_TIMEOUT = 1;
	private static final int CONNECTION_TIMEOUT = 2;

	/**
	 * Prevent instantiation
	 */
	private GUITestDataProposalService() {

	}

	/**
	 * Search appropriate test data proposals for a given form field
	 * @param input
	 * @param formField
	 * @param connectToTargetDB
	 * @return a list containing test data proposals
	 */
	public static List<String> searchProposals(String input, FormField formField, boolean connectToTargetDB) {
		return searchProposals(input, formField.getDTOAttribute(), connectToTargetDB);
	}

	/**
	 * Search appropriate test data proposals for a given table column
	 * @param input
	 * @param column
	 * @param connectToTargetDB
	 * @return a list containing test data proposals
	 */
	public static List<String> searchProposals(String input, TableColumnField column, boolean connectToTargetDB) {
		return searchProposals(input, column.getDTOAttribute(), connectToTargetDB);
	}

	/**
	 * Search appropriate test data proposals for a given DTO attribute
	 * @param input
	 * @param dtoAttr
	 * @param connectToTargetDB
	 * @return a list containing test data proposals
	 */
	public static List<String> searchProposals(String input, DTOBeanAttribute dtoAttr, boolean connectToTargetDB) {
		try {
			// Search for proposals in existing tests
			final Set<String> proposals = searchProposalsInExistingTests(input, dtoAttr);

			if (!connectToTargetDB || proposals.size() >= MAX_RESULTS)
				return proposals.stream().toList();

			// Add proposals from the database
			for (final String proposal : searchProposalsInDatabase(input, dtoAttr)) {
				proposals.add(proposal);

				if (proposals.size() == MAX_RESULTS)
					break;
			}

			return proposals.stream().toList();
		}
		catch (final Exception e) {
			CodeCadenzaToolsPlugin.getInstance().logError(e);
		}

		return Collections.emptyList();
	}

	/**
	 * Search for appropriate test data proposals in all existing test cases
	 * @param project
	 * @param input
	 * @return a list containing test data proposals
	 */
	public static List<String> searchProposals(Project project, String input) {
		try {
			final var proposals = new HashSet<String>();

			// Iterate over all test cases and their actions
			for (final GUITestCase testCase : project.getAllGUITestCases())
				for (final GUITestAction testAction : testCase.getTestActions())
					for (final GUITestData testData : testAction.getTestData()) {
						if (testData.getNewValue() != null && testData.getNewValue().toLowerCase().startsWith(input.toLowerCase()))
							proposals.add(testData.getNewValue());

						if (testData.getExpectedValue() != null && testData.getExpectedValue().toLowerCase().startsWith(input.toLowerCase()))
							proposals.add(testData.getExpectedValue());

						if (testData.getFilterValue() != null && testData.getFilterValue().toLowerCase().startsWith(input.toLowerCase()))
							proposals.add(testData.getFilterValue());

						// Stop searching for further proposals as soon as enough values have been found!
						if (proposals.size() >= MAX_RESULTS)
							return proposals.stream().toList();
					}

			return proposals.stream().toList();
		}
		catch (final Exception e) {
			CodeCadenzaToolsPlugin.getInstance().logError(e);
		}

		return Collections.emptyList();
	}

	/**
	 * Test if the service is able to connect to the project's target database
	 * @param project
	 * @return true if a connection to the target database could be established
	 */
	public static boolean enableDatabaseLookup(Project project) {
		// Define a function that should perform the connection test
		final Callable<Boolean> runnableTask = () -> {
			try (var dbManager = new DBManager(project)) {
				// Get the catalog in order to test the connection
				dbManager.getConnection().getCatalog();

				return true;
			}
			catch (final Exception e) {
				CodeCadenzaToolsPlugin.getInstance().logError(e);
			}

			return false;
		};

		try (final ExecutorService executorService = Executors.newSingleThreadExecutor()) {
			// Execute the connection test function
			final Future<Boolean> result = executorService.submit(runnableTask);

			try {
				// Don't wait too long in order to avoid freezing the GUI!
				return result.get(CONNECTION_TIMEOUT, TimeUnit.SECONDS);
			}
			catch (final InterruptedException _) {
				Thread.currentThread().interrupt();

				CodeCadenzaToolsPlugin.getInstance().logInfo("Operation has been interrupted!");
			}
			catch (ExecutionException | TimeoutException _) {
				CodeCadenzaToolsPlugin.getInstance().logInfo("Database connection test timed out!");
			}

			return false;
		}
	}

	/**
	 * Search for proposals in target database
	 * @param input
	 * @param dtoAttr
	 * @return a set containing the proposals
	 */
	private static Set<String> searchProposalsInDatabase(String input, DTOBeanAttribute dtoAttr) {
		final Project project = dtoAttr.getDTOBean().getNamespace().getProject();

		// Define a function that is responsible for searching proposals within the database
		final Callable<Set<String>> runnableTask = () -> {
			final var proposalList = new HashSet<String>();

			try (var dbManager = new DBManager(project);
					Connection con = dbManager.getConnection();
					Statement st = con.createStatement()) {
				// Build the select statement
				final String queryStatement = buildSelectStatement(input, dtoAttr);

				// If the query statement is empty it shouldn't be processed!
				if (queryStatement.isEmpty())
					return Collections.emptySet();

				// Limit the number of records that a query should return
				st.setMaxRows(MAX_RESULTS);

				// Execute the query
				try (ResultSet resultSet = st.executeQuery(queryStatement)) {
					// Add the values to the proposal list
					while (resultSet.next())
						proposalList.add(resultSet.getString(1));
				}

				return proposalList;
			}
			catch (final Exception e) {
				CodeCadenzaToolsPlugin.getInstance().logError(e);
			}

			return Collections.emptySet();
		};

		try (final ExecutorService executorService = Executors.newSingleThreadExecutor()) {
			// Execute the proposal function
			final Future<Set<String>> result = executorService.submit(runnableTask);

			try {
				// Don't wait too long in order to avoid freezing the GUI!
				return result.get(LOOKUP_TIMEOUT, TimeUnit.SECONDS);
			}
			catch (final InterruptedException _) {
				Thread.currentThread().interrupt();

				CodeCadenzaToolsPlugin.getInstance().logInfo("Operation was interrupted!");
			}
			catch (ExecutionException | TimeoutException _) {
				CodeCadenzaToolsPlugin.getInstance().logInfo("Database lookup operation timed out!");
			}

			return Collections.emptySet();
		}
	}

	/**
	 * Search proposals for a given DTO attribute in existing tests
	 * @param input
	 * @param attr
	 * @return a set containing the proposals
	 */
	private static Set<String> searchProposalsInExistingTests(String input, DTOBeanAttribute attr) {
		final Project project = attr.getDTOBean().getNamespace().getProject();
		final var proposals = new HashSet<String>();

		// Iterate over all test cases and their actions
		for (final GUITestCase testCase : project.getAllGUITestCases())
			for (final GUITestAction testAction : testCase.getTestActions())
				for (final GUITestData testData : testAction.getTestData()) {
					if (testData.getTableColumnField() == null && testData.getFormField() == null)
						continue;

					DTOBeanAttribute dtoAttr = null;
					boolean addTestData = false;

					if (testData.getTableColumnField() != null)
						dtoAttr = testData.getTableColumnField().getDTOAttribute();

					if (testData.getFormField() != null)
						dtoAttr = testData.getFormField().getDTOAttribute();

					final DTOBean refDTO = dtoAttr.getReferencedDTOBean();

					if (attr.getDomainAttribute() != null) {
						if (dtoAttr.getDomainAttribute() != null && dtoAttr.getDomainAttribute().equals(attr.getDomainAttribute()))
							addTestData = true;
					}
					else if (refDTO != null && refDTO.equals(attr.getReferencedDTOBean()))
						addTestData = true;

					if (addTestData) {
						if (testData.getNewValue() != null && testData.getNewValue().toLowerCase().startsWith(input.toLowerCase()))
							proposals.add(testData.getNewValue());

						if (testData.getExpectedValue() != null && testData.getExpectedValue().toLowerCase().startsWith(input.toLowerCase()))
							proposals.add(testData.getExpectedValue());

						if (testData.getFilterValue() != null && testData.getFilterValue().toLowerCase().startsWith(input.toLowerCase()))
							proposals.add(testData.getFilterValue());
					}

					// Stop searching for further proposals as soon as enough values have been found!
					if (proposals.size() >= MAX_RESULTS)
						return proposals;
				}

		return proposals;
	}

	/**
	 * @param input
	 * @param dtoAttr
	 * @return the generated query statement
	 */
	private static String buildSelectStatement(String input, DTOBeanAttribute dtoAttr) {
		final var b = new StringBuilder();
		DBColumn column = null;
		DBTable table = null;
		boolean isFilteringSupported = true;

		if (dtoAttr == null)
			return b.toString();

		if (dtoAttr.getDomainAttribute() != null) {
			if (!dtoAttr.getDomainAttribute().isPersistent())
				return b.toString();

			column = dtoAttr.getDomainAttribute().getColumn();
			table = column.getDatabaseTable();
			isFilteringSupported = dtoAttr.getDomainAttribute().isWildcardFilteringSupported();

			// If the domain attribute belongs to a mapped superclass the respective table from the domain model must not be used!
			if (dtoAttr.getDomainAttribute().getDomainObject().isMappedSuperClass())
				table = determineDatabaseTable(dtoAttr.getDTOBean().getDomainObject());
		}
		else if (dtoAttr.getReferencedDTOBean() != null) {
			table = determineDatabaseTable(dtoAttr.getReferencedDTOBean().getDomainObject());
			column = table.getPrimaryKey().getColumn();

			if (dtoAttr.getReferencedDTOBean().getDisplayAttribute() != null)
				column = dtoAttr.getReferencedDTOBean().getDisplayAttribute().getDomainAttribute().getColumn();
			else
				isFilteringSupported = dtoAttr.getReferencedDTOBean().getPKAttribute().getDomainAttribute()
						.isWildcardFilteringSupported();
		}

		if (table != null && column != null) {
			b.append("select distinct " + column.getConvertedName() + " ");
			b.append("from " + table.getFullDatabaseName() + " ");
			b.append("where " + column.getConvertedName() + " ");

			if (isFilteringSupported)
				b.append("like '" + input + "%' ");
			else
				b.append("= '" + input + "' ");

			b.append("order by " + column.getConvertedName());
		}

		return b.toString();
	}

	/**
	 * @param domainObject
	 * @return the database table for a given domain object
	 * @throws IllegalStateException if the table could not be determined
	 */
	private static DBTable determineDatabaseTable(DomainObject domainObject) {
		DBTable table = domainObject.getDatabaseTable();

		// If the domain object doesn't reference a database table we must search for the table of the root domain object!
		if (table == null)
			table = domainObject.getRootParentDomainObject(false).getDatabaseTable();

		if (table != null)
			return table;

		throw new IllegalStateException(
				"The database table for domain object '" + domainObject.getName() + "' could not be determined!");
	}

}
