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
package net.codecadenza.eclipse.generator.testing.integration;

import static net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation.PLACEHOLDER_PATTERN;
import static net.codecadenza.eclipse.model.testing.TestDataAttribute.ATTRIBUTE_NAME_FILTER_CRITERIA;
import static net.codecadenza.eclipse.model.testing.TestDataAttribute.ATTRIBUTE_NAME_SEARCH_FIELDS;
import static net.codecadenza.eclipse.model.testing.TestDataAttribute.ATTRIBUTE_NAME_SORT_ORDER;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.regex.Matcher;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.testing.AssertionOperator;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.MethodInvocationParameter;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.apache.commons.text.StringEscapeUtils;

/**
 * <p>
 * Generator for integration test data files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationTestDataGenerator {
	private static final int INITAL_INDENT = 3;

	private final Map<TestDataObject, UUID> objectIdRegistry = new HashMap<>();
	private final IntegrationTestCase testCase;
	private final Project project;

	/**
	 * Constructor
	 * @param testCase
	 */
	public IntegrationTestDataGenerator(IntegrationTestCase testCase) {
		this.testCase = testCase;
		this.project = testCase.getNamespace().getProject();
	}

	/**
	 * Create or update the test data XML file for a given test case
	 * @throws Exception if the generation of the test data file has failed
	 */
	public void createFile() throws Exception {
		final var path = project.getTestDataFolder() + "/" + testCase.getName() + ".xml";
		final BuildArtifactType artifactType = testCase.getTestModule().getArtifactType();
		final var dataFile = new WorkspaceFile(project, artifactType, path, createContent());

		EclipseIDEService.createOrUpdateFile(dataFile);
	}

	/**
	 * Create the test data XML file
	 * @return the generated content
	 */
	private String createContent() {
		final var b = new StringBuilder();
		b.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
		b.append("<test_data");

		if (testCase.addCredentials()) {
			b.append(" user_name=\"" + StringEscapeUtils.escapeXml10(testCase.getUserName()) + "\"");
			b.append(" password=\"" + StringEscapeUtils.escapeXml10(testCase.getPassword()) + "\"");
		}

		b.append(">\n");

		for (final IntegrationMethodTestInvocation invocation : testCase.getMethodInvocations()) {
			UUID groupId = null;

			if (!invocation.getNestedInvocations().isEmpty())
				groupId = UUID.randomUUID();

			b.append(generateMethodInvocation(null, invocation, groupId));

			for (final IntegrationMethodTestInvocation nestedInvocation : invocation.getNestedInvocations())
				b.append(generateMethodInvocation(invocation, nestedInvocation, groupId));
		}

		b.append("</test_data>");

		return b.toString();
	}

	/**
	 * Generate the content for the given {@link IntegrationMethodTestInvocation}
	 * @param parentInvocation the optional parent invocation
	 * @param invocation the test invocation
	 * @param groupId the group ID
	 * @return the generated content
	 */
	private String generateMethodInvocation(IntegrationMethodTestInvocation parentInvocation,
			IntegrationMethodTestInvocation invocation, UUID groupId) {
		final var b = new StringBuilder();
		b.append("\t<method_invocation ");
		b.append("class_name=\"" + invocation.getIntegrationMethod().getIntegrationBean().getClientClassName() + "\" ");
		b.append("method_name=\"" + invocation.getIntegrationMethod().getName() + "\"");

		if (parentInvocation != null) {
			if (parentInvocation.getTimeout() != null)
				b.append(" timeout_millis=\"" + parentInvocation.getTimeout() + "\"");
		}
		else if (invocation.getTimeout() != null)
			b.append(" timeout_millis=\"" + invocation.getTimeout() + "\"");

		if (groupId != null)
			b.append(" group_id=\"" + groupId + "\"");

		if (invocation.getExpectedSize() != null)
			b.append(" expected_size=\"" + invocation.getExpectedSize() + "\"");

		b.append(">\n");

		if (!invocation.getParameters().isEmpty()) {
			b.append("\t\t<parameters>\n");

			for (final MethodInvocationParameter param : invocation.getParameters())
				generateParameter(b, param, INITAL_INDENT);

			b.append("\t\t</parameters>\n");
		}
		else
			b.append("\t\t<parameters/>\n");

		if (!invocation.getReturnValues().isEmpty()) {
			b.append("\t\t<return_value>\n");

			for (final TestDataObject returnValue : invocation.getReturnValues())
				generateTestDataObject(b, returnValue, "returnValue", INITAL_INDENT);

			b.append("\t\t</return_value>\n");
		}

		String postProcessingStatement = null;

		if (parentInvocation != null) {
			if (parentInvocation.getPostProcessingStatement() != null)
				postProcessingStatement = parentInvocation.getPostProcessingStatement();
		}
		else
			postProcessingStatement = invocation.getPostProcessingStatement();

		if (postProcessingStatement != null && !postProcessingStatement.isEmpty()) {
			final Matcher matcher = PLACEHOLDER_PATTERN.matcher(postProcessingStatement);
			final String jdbcStatement = matcher.replaceAll("?");

			b.append("\t\t<post_processing_statement>\n");
			b.append("\t\t\t" + StringEscapeUtils.escapeXml10(jdbcStatement) + "\n");
			b.append("\t\t</post_processing_statement>\n");
		}

		b.append("\t</method_invocation>\n");

		return b.toString();
	}

	/**
	 * Generate the content for a given method parameter
	 * @param b the {@link StringBuilder} to add the content to
	 * @param param the method parameter
	 * @param indentLevel the indent level
	 */
	private void generateParameter(StringBuilder b, MethodInvocationParameter param, int indentLevel) {
		final String indent = "\t".repeat(indentLevel);

		b.append(indent).append("<parameter name=\"" + param.getName() + "\">\n");

		for (final TestDataObject obj : param.getParameterValues())
			generateTestDataObject(b, obj, param.getName(), indentLevel + 1);

		b.append(indent).append("</parameter>\n");
	}

	/**
	 * Generate the content for a {@link TestDataObject}
	 * @param b the {@link StringBuilder} to add the content to
	 * @param testDataObject the test data object
	 * @param name the name of the test object if it is not mapped to a {@link MappingObject}
	 * @param indentLevel the indent level
	 */
	private void generateTestDataObject(StringBuilder b, TestDataObject testDataObject, String name, int indentLevel) {
		final String indent = "\t".repeat(indentLevel);
		final UUID newId = UUID.randomUUID();
		final boolean addSingleAttribute = testDataObject.getMappingObject() == null && testDataObject.getAttributes().size() == 1;

		objectIdRegistry.put(testDataObject, newId);

		b.append(indent).append("<object id=\"" + newId.toString() + "\"");

		// A test data object can only initialized with a value if it contains a single attribute!
		if (addSingleAttribute) {
			final TestDataAttribute attr = testDataObject.getAttributes().getFirst();

			if (attr.getValue() != null) {
				b.append(" value=\"" + StringEscapeUtils.escapeXml10(attr.getValue()) + "\"");

				if (name != null && !name.isEmpty())
					b.append(" name=\"" + name + "\"");
			}
			else if (attr.getReferencedAttribute() != null)
				b.append(" ref_field_id=\"" + attr.getReferencedAttribute().getId() + "\"");
		}

		// Handle object references
		if (testDataObject.getReferencedObject() != null) {
			final UUID refId = objectIdRegistry.get(testDataObject);

			if (objectIdRegistry.values().stream().noneMatch(v -> v.equals(refId)))
				throw new IllegalStateException("Referenced object with ID " + refId + " does not exist yet!");

			b.append(" ref_id=\"" + refId + "\"");
		}

		if (addSingleAttribute) {
			final TestDataAttribute attr = testDataObject.getAttributes().getFirst();

			if (attr.isTrackValue()) {
				b.append(">\n");

				// If the attribute is tracked, a dedicated field element must be added so that the generated value can be set correctly!
				generateTestDataAttribute(b, attr, indentLevel + 1);

				b.append(indent).append("</object>\n");
			}
			else
				b.append("/>\n");

			return;
		}

		b.append(">\n");

		// Generate fields
		for (final TestDataAttribute testDataAttribute : testDataObject.getAttributes()) {
			final AssertionOperator operator = testDataAttribute.getOperator();

			if (testDataAttribute.skip() && !testDataAttribute.isTrackValue()
					|| (operator == AssertionOperator.IS_NULL || operator == AssertionOperator.IS_NOT_NULL))
				continue;

			generateTestDataAttribute(b, testDataAttribute, indentLevel + 1);
		}

		b.append(indent).append("</object>\n");
	}

	/**
	 * Generate the content for a {@link TestDataAttribute}
	 * @param b the {@link StringBuilder} to add the content to
	 * @param testDataAttribute the test data attribute
	 * @param indentLevel the indent level
	 */
	private void generateTestDataAttribute(StringBuilder b, TestDataAttribute testDataAttribute, int indentLevel) {
		final String indent = "\t".repeat(indentLevel);

		b.append(indent);
		b.append("<field");

		if (testDataAttribute.getId() != null)
			b.append(" id=\"" + testDataAttribute.getId() + "\"");

		b.append(" name=\"");

		if (testDataAttribute.getName() != null && !testDataAttribute.getName().isEmpty())
			b.append(testDataAttribute.getName());
		else if (testDataAttribute.getMappingAttribute() != null)
			b.append(testDataAttribute.getMappingAttribute().getName());

		b.append("\"");

		if (testDataAttribute.getExpectedSize() != null)
			b.append(" expected_size=\"" + testDataAttribute.getExpectedSize() + "\"");

		if (testDataAttribute.getValue() != null)
			b.append(" value=\"" + StringEscapeUtils.escapeXml10(testDataAttribute.getValue()) + "\"");
		else if (testDataAttribute.getReferencedAttribute() != null)
			b.append(" ref_field_id=\"" + testDataAttribute.getReferencedAttribute().getId() + "\"");

		if (!testDataAttribute.getReferencedObjects().isEmpty()) {
			b.append(">\n");

			final boolean isSearchField = ATTRIBUTE_NAME_SEARCH_FIELDS.equals(testDataAttribute.getName());

			for (final TestDataObject refObject : testDataAttribute.getReferencedObjects()) {
				// Skip the generation of an empty SearchInputField
				if (isSearchField && skipSearchInputField(refObject))
					continue;

				generateTestDataObject(b, refObject, null, indentLevel + 1);
			}

			b.append(indent);
			b.append("</field>\n");
		}
		else
			b.append("/>\n");
	}

	/**
	 * @param testDataObject
	 * @return true if the {@link TestDataObject} that represents a search input field can be skipped
	 */
	private boolean skipSearchInputField(TestDataObject testDataObject) {
		final TestDataAttribute filterAttribute = testDataObject.getAttributeByName(ATTRIBUTE_NAME_FILTER_CRITERIA);
		final TestDataAttribute sortOrderAttribute = testDataObject.getAttributeByName(ATTRIBUTE_NAME_SORT_ORDER);

		return filterAttribute != null && sortOrderAttribute != null && filterAttribute.getValue() == null
				&& sortOrderAttribute.getValue() == null && filterAttribute.getReferencedAttribute() == null;
	}

}
