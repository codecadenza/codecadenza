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
package net.codecadenza.eclipse.service.testing.integration;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.generator.testing.integration.IntegrationTestCaseGenerator;
import net.codecadenza.eclipse.generator.testing.integration.IntegrationTestDataGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DirectExchangeMode;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.integration.IntegrationMethodParameter;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation;
import net.codecadenza.eclipse.model.testing.IntegrationTestCase;
import net.codecadenza.eclipse.model.testing.IntegrationTestModule;
import net.codecadenza.eclipse.model.testing.MethodInvocationParameter;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import net.codecadenza.eclipse.model.testing.TestDataObject;
import net.codecadenza.eclipse.model.testing.TestingFactory;
import net.codecadenza.eclipse.shared.Constants;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Service for integration test cases
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class IntegrationTestCaseService {
	private final IntegrationTestModule testModule;
	private final Project project;

	/**
	 * Constructor
	 * @param testModule
	 */
	public IntegrationTestCaseService(IntegrationTestModule testModule) {
		this.testModule = testModule;
		this.project = testModule.getProject();
	}

	/**
	 * Rebuild the test case source file and the corresponding test data file
	 * @param testCase
	 * @throws Exception if the rebuild operation has failed due to an internal error
	 */
	public void rebuildTestCaseSourceFiles(IntegrationTestCase testCase) throws Exception {
		new IntegrationTestDataGenerator(testCase).createFile();
		new IntegrationTestCaseGenerator(testCase).createSourceFile();
	}

	/**
	 * Delete the given test case and remove respective files
	 * @param testCase
	 * @throws Exception if the delete operation has failed due to an internal error
	 */
	public void deleteTestCase(IntegrationTestCase testCase) throws Exception {
		final BuildArtifactType artifactType = testCase.getTestModule().getArtifactType();

		// Remove the test case from the meta-model
		project.eResource().getContents().remove(testCase);

		EclipseIDEService.saveProjectMetaData(project);

		// Delete the test case source file
		EclipseIDEService.deleteSource(testCase.getSourceFile());

		// Delete the test data file
		final String projectName = project.getTargetProjectName(artifactType);
		final var path = project.getTestDataFolder() + "/" + testCase.getName() + ".xml";

		EclipseIDEService.deleteWorkspaceFile(projectName, path);
	}

	/**
	 * Rename the given test case
	 * @param testCase
	 * @param newName
	 * @throws Exception if the rename operation has failed
	 */
	public void renameTestCase(IntegrationTestCase testCase, String newName) throws Exception {
		final BuildArtifactType artifactType = testCase.getTestModule().getArtifactType();

		// Rename the test case source file
		EclipseIDEService.renameCompUnit(testCase.getSourceFile(), newName);

		// Delete the test data file
		final var path = project.getTestDataFolder() + "/" + testCase.getName() + ".xml";
		final String projectName = project.getTargetProjectName(artifactType);

		EclipseIDEService.deleteWorkspaceFile(projectName, path);

		testCase.setName(newName);

		// Create a new test data file
		new IntegrationTestDataGenerator(testCase).createFile();
	}

	/**
	 * Initialize an {@link IntegrationMethodTestInvocation} based on the given integration method
	 * @param testCase the integration test case
	 * @param integrationMethod the integration method
	 * @param parentInvocation the optional parent invocation
	 * @return a new {@link IntegrationMethodTestInvocation}
	 */
	public IntegrationMethodTestInvocation initMethodInvocation(IntegrationTestCase testCase,
			AbstractIntegrationMethod integrationMethod, IntegrationMethodTestInvocation parentInvocation) {
		final BoundaryMethod boundaryMethod = integrationMethod.getBoundaryMethod();
		final BoundaryMethodTypeEnumeration methodType = boundaryMethod.getMethodType();

		final IntegrationMethodTestInvocation invocation = TestingFactory.eINSTANCE.createIntegrationMethodTestInvocation();
		invocation.setIntegrationMethod(integrationMethod);
		invocation.setTestMethodName(integrationMethod.getName());

		for (final IntegrationMethodParameter parameter : integrationMethod.getIntegrationParameters()) {
			JavaType paramType = null;
			boolean representsList = false;
			TestDataObject testObject = null;

			if ((methodType == BoundaryMethodTypeEnumeration.SEARCH || methodType == BoundaryMethodTypeEnumeration.COUNT)
					&& parameter.getName().equals(Constants.SEARCH_PARAM_NAME))
				testObject = initSearchInput(integrationMethod);
			else {
				if (parameter.getMethodParameter() != null) {
					paramType = parameter.getMethodParameter().getType();
					representsList = parameter.getMethodParameter().getModifier() != JavaTypeModifierEnumeration.NONE;
				}
				else
					paramType = project.getJavaTypeByName(parameter.getType());

				if (paramType == null) {
					// Ignore the AsyncResponse parameter of a REST method
					if ("response".equals(parameter.getName())
							&& integrationMethod.getIntegrationBean().getIntegrationTechnology() == IntegrationTechnology.REST)
						continue;

					throw new IllegalStateException(
							"Cannot add parameter '" + parameter.getName() + "' with unsupported type '" + parameter.getType() + "'!");
				}

				// Just search for a tracking attribute if the method returns void in order to prevent adding the same tracking attribute
				// twice for the same method!
				testObject = initTestObject(paramType, boundaryMethod, invocation.isReturnVoid(), true);
			}

			if (methodType == BoundaryMethodTypeEnumeration.FIND_BY_ID || methodType == BoundaryMethodTypeEnumeration.COPY
					|| methodType == BoundaryMethodTypeEnumeration.DELETE || methodType == BoundaryMethodTypeEnumeration.EXISTS_BY_ID
					|| methodType == BoundaryMethodTypeEnumeration.FIND_BY_PARENT || methodType == BoundaryMethodTypeEnumeration.DOWNLOAD)
				for (final TestDataAttribute testDataAttribute : testObject.getAttributes())
					initReferencedAttribute(testCase, invocation, testDataAttribute, parentInvocation);

			final MethodInvocationParameter invocationParameter = TestingFactory.eINSTANCE.createMethodInvocationParameter();
			invocationParameter.setType(paramType);
			invocationParameter.setName(parameter.getName());
			invocationParameter.setRepresentsList(representsList);
			invocationParameter.getParameterValues().add(testObject);

			invocation.getParameters().add(invocationParameter);
		}

		// Do not initialize a test data object if the method returns a list!
		if (!invocation.isDownloadFile() && !invocation.isReturnVoid()
				&& integrationMethod.getReturnTypeModifier() == JavaTypeModifierEnumeration.NONE)
			invocation.getReturnValues().add(initTestObject(integrationMethod.getReturnType(), boundaryMethod, true, false));

		if (invocation.isReturnVoid() && !invocation.getTrackedAttributes().isEmpty())
			addPostProcessingStatement(invocation);

		return invocation;
	}

	/**
	 * Create a new {@link TestDataObject} based on the given {@link JavaType}
	 * @param objectType the type of the object
	 * @param boundaryMethod the corresponding boundary method
	 * @param searchTrackingAttribute flag that controls if tracking attributes should be searched
	 * @param allowSkipAttributes flag that controls if attributes that are managed by the persistence provider should be skipped
	 * @return a new {@link TestDataObject}
	 */
	public TestDataObject initTestObject(final JavaType objectType, BoundaryMethod boundaryMethod, boolean searchTrackingAttribute,
			boolean allowSkipAttributes) {
		final BoundaryMethodTypeEnumeration methodType = boundaryMethod.getMethodType();
		final TestDataObject testDataObject = TestingFactory.eINSTANCE.createTestDataObject();
		final List<TestDataAttribute> attributes = new ArrayList<>();

		if (objectType instanceof final DTOBean dto) {
			testDataObject.setMappingObject(dto);

			for (final MappingAttribute mappingAttribute : dto.getAttributes()) {
				final TestDataAttribute testDataAttribute = initAttribute(mappingAttribute, boundaryMethod, searchTrackingAttribute,
						allowSkipAttributes);

				if (testDataAttribute != null)
					attributes.add(testDataAttribute);
			}
		}
		else if (objectType instanceof final ExchangeMappingObject exchangeMappingObject) {
			testDataObject.setMappingObject(exchangeMappingObject);

			for (final MappingAttribute mappingAttribute : exchangeMappingObject.getAttributes()) {
				final TestDataAttribute testDataAttribute = initAttribute(mappingAttribute, boundaryMethod, searchTrackingAttribute,
						allowSkipAttributes);

				if (testDataAttribute != null)
					attributes.add(testDataAttribute);
			}
		}
		else {
			final TestDataAttribute testDataAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
			testDataAttribute.setMappingType(objectType);

			// A copy operation always returns a generated value - no matter what type the ID has!
			if (methodType == BoundaryMethodTypeEnumeration.COPY && searchTrackingAttribute) {
				testDataAttribute.setId(UUID.randomUUID().toString());
				testDataAttribute.setTrackValue(true);
			}

			attributes.add(testDataAttribute);
		}

		testDataObject.getAttributes().addAll(attributes.stream().sorted(new TestDataAttributeComparator()).toList());

		return testDataObject;
	}

	/**
	 * Create and initialize a new {@link TestDataAttribute}
	 * @param mappingAttribute the mapping attribute
	 * @param boundaryMethod the boundary method
	 * @param searchTrackingAttribute flag that controls if tracking attributes should be searched
	 * @param allowSkipAttributes flag that controls if attributes that are managed by the persistence provider should be skipped
	 * @return the new {@link TestDataAttribute} or null if it is not necessary
	 */
	private TestDataAttribute initAttribute(MappingAttribute mappingAttribute, BoundaryMethod boundaryMethod,
			boolean searchTrackingAttribute, boolean allowSkipAttributes) {
		final boolean trackAttribute = isTrackAttribute(mappingAttribute, boundaryMethod);

		if (allowSkipAttributes && (skipAttribute(mappingAttribute, boundaryMethod, searchTrackingAttribute)))
			return null;

		final TestDataAttribute testDataAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
		testDataAttribute.setMappingAttribute(mappingAttribute);

		if (searchTrackingAttribute && trackAttribute) {
			testDataAttribute.setTrackValue(isTrackAttribute(mappingAttribute, boundaryMethod));
			testDataAttribute.setId(UUID.randomUUID().toString());
		}

		return testDataAttribute;
	}

	/**
	 * Initialize a referenced {@link TestDataObject} based on the given {@link TestDataAttribute} and add it to the attribute's
	 * list of the referenced objects. It is assumed that the corresponding attribute references a proper type!
	 * @param testDataAttribute
	 * @return a new {@link TestDataObject}
	 */
	public TestDataObject initReferencedTestObject(TestDataAttribute testDataAttribute) {
		final TestDataObject testDataObject = TestingFactory.eINSTANCE.createTestDataObject();
		final List<TestDataAttribute> attributes = new ArrayList<>();

		if (testDataAttribute.getMappingAttribute() instanceof final DTOBeanAttribute refAttribute) {
			final DTOBean parentObject = refAttribute.getReferencedDTOBean();
			testDataObject.setMappingObject(parentObject);

			for (final DTOBeanAttribute dtoAttribute : parentObject.getAttributes()) {
				final TestDataAttribute testAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
				testAttribute.setMappingAttribute(dtoAttribute);

				attributes.add(testAttribute);
			}
		}
		else if (testDataAttribute.getMappingAttribute() instanceof final ExchangeMappingAttribute refAttribute) {
			final ExchangeMappingObject parentObject = refAttribute.getExchangeMappingObject();
			testDataObject.setMappingObject(parentObject);

			for (final ExchangeMappingAttribute attribute : parentObject.getAttributes()) {
				final TestDataAttribute testAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
				testAttribute.setMappingAttribute(attribute);

				attributes.add(testAttribute);
			}
		}
		else
			throw new IllegalStateException("The given type cannot be used to initialize a new test data object!");

		testDataObject.getAttributes().addAll(attributes.stream().sorted(new TestDataAttributeComparator()).toList());

		testDataAttribute.getReferencedObjects().add(testDataObject);

		return testDataObject;
	}

	/**
	 * Check if the attribute should be tracked
	 * @param mappingAttribute
	 * @param boundaryMethod
	 * @return true if the attribute should be tracked
	 */
	private boolean isTrackAttribute(MappingAttribute mappingAttribute, BoundaryMethod boundaryMethod) {
		final BoundaryMethodTypeEnumeration methodType = boundaryMethod.getMethodType();
		final DomainAttribute domainAttribute = mappingAttribute.getDomainAttribute();

		if (domainAttribute != null && domainAttribute.isPk() && mappingAttribute.getAssociation() == null
				&& (methodType == BoundaryMethodTypeEnumeration.CREATE || methodType == BoundaryMethodTypeEnumeration.SAVE
						|| (methodType == BoundaryMethodTypeEnumeration.UPLOAD_IMPORT
								&& boundaryMethod.getServiceMethod() instanceof final DataExchangeMethod exchangeMethod
								&& exchangeMethod.isProcessSingleObject() && exchangeMethod.getExchangeMode() instanceof DirectExchangeMode))) {
			return domainAttribute.getDomainObject().getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE;
		}

		return false;
	}

	/**
	 * Check whether the specified attribute does not need to be added to the {@link TestDataObject}, since the corresponding data
	 * is generated by the persistence provider and manual entry therefore makes no sense
	 * @param mappingAttribute the mapping attribute
	 * @param boundaryMethod the boundary method
	 * @param searchTrackingAttribute flag that controls if tracking attributes should be searched
	 * @return true if the attribute can be skipped
	 */
	private boolean skipAttribute(MappingAttribute mappingAttribute, BoundaryMethod boundaryMethod,
			boolean searchTrackingAttribute) {
		final BoundaryMethodTypeEnumeration methodType = boundaryMethod.getMethodType();
		final DomainAttribute domainAttribute = mappingAttribute.getDomainAttribute();

		return domainAttribute != null && ((domainAttribute.isSetDateOnPersist() || domainAttribute.isSetDateOnUpdate())
				|| (methodType == BoundaryMethodTypeEnumeration.CREATE && domainAttribute.isTrackVersion())
				|| (methodType == BoundaryMethodTypeEnumeration.CREATE && !searchTrackingAttribute
						&& isTrackAttribute(mappingAttribute, boundaryMethod)));
	}

	/**
	 * Search for a tracking attribute that can be referenced
	 * @param testCase
	 * @param methodInvocation
	 * @param testDataAttribute
	 * @param parentInvocation
	 */
	private void initReferencedAttribute(IntegrationTestCase testCase, IntegrationMethodTestInvocation methodInvocation,
			TestDataAttribute testDataAttribute, IntegrationMethodTestInvocation parentInvocation) {
		final List<IntegrationMethodTestInvocation> invocations = new ArrayList<>(testCase.getMethodInvocations());
		invocations.add(methodInvocation);

		// Search for previous invocations that track the respective attribute
		for (final IntegrationMethodTestInvocation previousInvocation : invocations) {
			for (final TestDataAttribute trackedAttribute : previousInvocation.getTrackedAttributes()) {
				if (methodInvocation.getIntegrationMethod().getIntegrationBean().getDomainObject()
						.equals(previousInvocation.getIntegrationMethod().getIntegrationBean().getDomainObject())
						&& trackedAttribute.getMappingAttribute() != null
						&& trackedAttribute.getMappingAttribute().getDomainAttribute().isPk()) {

					if (parentInvocation != null) {
						// Try to bind the generated values in the "correct" order in subsequent invocations
						final int index = parentInvocation.getNestedInvocations().size();

						if (index <= previousInvocation.getNestedInvocations().size() - 1) {
							final IntegrationMethodTestInvocation nestedInvocation = previousInvocation.getNestedInvocations().get(index);

							for (final TestDataAttribute attribute : nestedInvocation.getTrackedAttributes()) {
								if (trackedAttribute.getMappingAttribute().equals(attribute.getMappingAttribute())) {
									testDataAttribute.setReferencedAttribute(attribute);
									return;
								}
							}
						}
					}
					else
						testDataAttribute.setReferencedAttribute(trackedAttribute);

					return;
				}
			}
		}
	}

	/**
	 * Initialize a {@link TestDataObject} for either a search or a count operation
	 * @param integrationMethod
	 * @return the initialized {@link TestDataObject}
	 */
	private TestDataObject initSearchInput(AbstractIntegrationMethod integrationMethod) {
		final BoundaryMethod boundaryMethod = integrationMethod.getBoundaryMethod();
		final BoundaryMethodTypeEnumeration methodType = boundaryMethod.getMethodType();
		final JavaType returnType;

		if (methodType == BoundaryMethodTypeEnumeration.COUNT)
			returnType = integrationMethod.getBoundaryMethod().getSearchMethod().getReturnType();
		else
			returnType = integrationMethod.getReturnType();

		// Apply the formatting rules from the test module to the search input object
		final TestDataAttribute dateTimeFormatAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
		dateTimeFormatAttribute.setName(TestDataAttribute.ATTRIBUTE_NAME_DATE_TIME_FORMAT);
		dateTimeFormatAttribute.setValue(testModule.getDateTimeFormat());
		dateTimeFormatAttribute.setMappingType(project.getJavaTypeByName(JavaType.STRING));

		final TestDataAttribute maxResultAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
		maxResultAttribute.setName(TestDataAttribute.ATTRIBUTE_NAME_MAX_RESULT);
		maxResultAttribute.setValue("50");
		maxResultAttribute.setMappingType(project.getJavaTypeByName(JavaType.INT));

		final TestDataAttribute dateFormatAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
		dateFormatAttribute.setName(TestDataAttribute.ATTRIBUTE_NAME_DATE_FORMAT);
		dateFormatAttribute.setValue(testModule.getDateFormat());
		dateFormatAttribute.setMappingType(project.getJavaTypeByName(JavaType.STRING));

		final TestDataAttribute numberFormatAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
		numberFormatAttribute.setName(TestDataAttribute.ATTRIBUTE_NAME_NUMBER_FORMAT);
		numberFormatAttribute.setValue(testModule.getDecimalFormat());
		numberFormatAttribute.setMappingType(project.getJavaTypeByName(JavaType.STRING));

		final TestDataAttribute decimalSeparatorAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
		decimalSeparatorAttribute.setName(TestDataAttribute.ATTRIBUTE_NAME_DECIMAL_SEPARATOR);
		decimalSeparatorAttribute.setValue(Character.toString(testModule.getDecimalSeparator()));
		decimalSeparatorAttribute.setMappingType(project.getJavaTypeByName(JavaType.CHAR));

		final TestDataAttribute groupingSeparatorAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
		groupingSeparatorAttribute.setName(TestDataAttribute.ATTRIBUTE_NAME_GROUPING_SEPARATOR);
		groupingSeparatorAttribute.setValue(Character.toString(testModule.getGroupingSeparator()));
		groupingSeparatorAttribute.setMappingType(project.getJavaTypeByName(JavaType.CHAR));

		final TestDataAttribute searchFieldsAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
		searchFieldsAttribute.setName(TestDataAttribute.ATTRIBUTE_NAME_SEARCH_FIELDS);

		final TestDataObject searchInput = TestingFactory.eINSTANCE.createTestDataObject();
		searchInput.getAttributes().add(maxResultAttribute);
		searchInput.getAttributes().add(dateTimeFormatAttribute);
		searchInput.getAttributes().add(dateFormatAttribute);
		searchInput.getAttributes().add(numberFormatAttribute);
		searchInput.getAttributes().add(decimalSeparatorAttribute);
		searchInput.getAttributes().add(groupingSeparatorAttribute);
		searchInput.getAttributes().add(searchFieldsAttribute);

		int sortIndex = 1;

		// Create a search filter attribute for every DTO attribute
		for (final TestDataAttribute testAttribute : initTestObject(returnType, boundaryMethod, false, false).getAttributes()) {
			final var dtoAttribute = (DTOBeanAttribute) testAttribute.getMappingAttribute();
			final JavaType type = testAttribute.getJavaType();

			final TestDataAttribute filterAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
			filterAttribute.setMappingAttribute(testAttribute.getMappingAttribute());
			filterAttribute.setName(TestDataAttribute.ATTRIBUTE_NAME_FILTER_CRITERIA);
			filterAttribute.setMappingType(project.getJavaTypeByName(JavaType.STRING));

			final TestDataAttribute nameAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
			nameAttribute.setName(TestDataAttribute.ATTRIBUTE_NAME_FIELD_NAME);
			nameAttribute.setMappingType(project.getJavaTypeByName(JavaType.STRING));
			nameAttribute.setValue(dtoAttribute.getName());

			final TestDataAttribute operatorAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
			operatorAttribute.setName(TestDataAttribute.ATTRIBUTE_NAME_FILTER_OPERATOR);
			operatorAttribute.setMappingType(project.getJavaTypeByName(JavaType.STRING));

			final TestDataAttribute hasDateTimeFormatAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
			hasDateTimeFormatAttribute.setName(TestDataAttribute.ATTRIBUTE_NAME_IS_DATE_TIME_FORMAT);
			hasDateTimeFormatAttribute.setMappingType(project.getJavaTypeByName(JavaType.BOOL));

			if (type.isLocalDate() || dtoAttribute.getDomainAttribute() != null
					&& dtoAttribute.getDomainAttribute().getTemporalType() == TemporalTypeEnumeration.DATE)
				hasDateTimeFormatAttribute.setValue(Boolean.toString(false));

			final TestDataObject searchField = TestingFactory.eINSTANCE.createTestDataObject();
			searchField.getAttributes().add(filterAttribute);
			searchField.getAttributes().add(nameAttribute);
			searchField.getAttributes().add(operatorAttribute);
			searchField.getAttributes().add(hasDateTimeFormatAttribute);

			if (methodType != BoundaryMethodTypeEnumeration.COUNT) {
				final TestDataAttribute sortOrderAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
				sortOrderAttribute.setName(TestDataAttribute.ATTRIBUTE_NAME_SORT_ORDER);
				sortOrderAttribute.setMappingType(project.getJavaTypeByName(JavaType.STRING));

				final TestDataAttribute sortIndexAttribute = TestingFactory.eINSTANCE.createTestDataAttribute();
				sortIndexAttribute.setName(TestDataAttribute.ATTRIBUTE_NAME_SORT_INDEX);
				sortIndexAttribute.setMappingType(project.getJavaTypeByName(JavaType.INT));
				sortIndexAttribute.setValue(Integer.toString(sortIndex++));

				searchField.getAttributes().add(sortOrderAttribute);
				searchField.getAttributes().add(sortIndexAttribute);
			}

			searchFieldsAttribute.getReferencedObjects().add(searchField);
		}

		return searchInput;
	}

	/**
	 * Add the post-processing SQL statement for fetching a generated column value
	 * @param invocation
	 */
	private void addPostProcessingStatement(IntegrationMethodTestInvocation invocation) {
		final AbstractIntegrationMethod integrationMethod = invocation.getIntegrationMethod();
		final DomainObject domainObject = integrationMethod.getIntegrationBean().getDomainObject();
		final Collection<DomainObject> inheritanceTree = domainObject.getFullInheritanceTree();
		final DomainAttribute displayAttribute = domainObject.getDisplayAttribute();
		final DBTable rootTable = domainObject.getRootParentDomainObject(false).getDatabaseTable();
		final String tableName = rootTable.getName();
		final String pkColumnName = domainObject.getPKAttribute().getColumn().getName();
		final var sqlCommand = new StringBuilder();
		char alias = 'a';

		if (displayAttribute != null) {
			final DBTable displayAttributeTable = displayAttribute.getDomainObject().getDatabaseTable() != null
					? displayAttribute.getDomainObject().getDatabaseTable() : rootTable;
			boolean firstCondition = true;

			final var tableMap = new HashMap<Character, DBTable>();
			tableMap.put(alias, rootTable);

			// Collect all tables that must be joined. Note, that the column of the display attribute can be contained in any table of a
			// joined inheritance hierarchy!
			for (final DomainObject parentDomainObject : inheritanceTree) {
				if (parentDomainObject.getDatabaseTable() == null
						|| displayAttributeTable.equals(parentDomainObject.getDatabaseTable())) {
					break;
				}

				alias += 1;
				tableMap.put(alias, parentDomainObject.getDatabaseTable());
			}

			final String tableList = tableMap.entrySet().stream().map(entry -> entry.getValue().getName() + " " + entry.getKey())
					.collect(Collectors.joining(", "));

			// Start from the initial alias again
			alias = 'a';

			sqlCommand.append("select " + alias + "." + pkColumnName + " from ");
			sqlCommand.append(tableList);
			sqlCommand.append(" where ");

			// Join the tables over their primary key columns
			for (final DomainObject parentDomainObject : inheritanceTree) {
				if (firstCondition)
					firstCondition = false;
				else
					sqlCommand.append(" and ");

				if (parentDomainObject.getDatabaseTable() == null
						|| displayAttributeTable.equals(parentDomainObject.getDatabaseTable())) {
					sqlCommand.append(alias + "." + displayAttribute.getColumn().getName() + " = :" + displayAttribute.getName());
					break;
				}

				sqlCommand.append(alias + "." + tableMap.get(alias++).getPrimaryKey().getColumn().getName() + " = ");
				sqlCommand.append(alias + "." + tableMap.get(alias).getPrimaryKey().getColumn().getName());
			}
		}
		else if (domainObject.getPKAttribute().getJavaType().isIntegerOrLong())
			sqlCommand.append("select max(" + alias + "." + pkColumnName + ") from " + tableName + " " + alias);
		else {
			// We can only provide an initial query at this point. It is the user's responsibility to adapt the query to find the
			// correct primary key value!
			sqlCommand.append("select " + alias + "." + pkColumnName + " from " + tableName + " " + alias);
		}

		invocation.setPostProcessingStatement(sqlCommand.toString());
	}

	/**
	 * Comparator for sorting {@link TestDataAttribute}s
	 */
	private static class TestDataAttributeComparator implements Comparator<TestDataAttribute>, Serializable {
		private static final long serialVersionUID = 7400090401064707729L;

		/*
		 * (non-Javadoc)
		 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
		 */
		@Override
		public int compare(TestDataAttribute testDataAttribute1, TestDataAttribute testDataAttribute2) {
			if (testDataAttribute1 == testDataAttribute2)
				return 0;

			if (testDataAttribute1 == null)
				return -1;

			if (testDataAttribute2 == null)
				return 1;

			final int rankingResult = Integer.compare(rank(testDataAttribute1), rank(testDataAttribute2));

			if (rankingResult != 0)
				return rankingResult;

			return safeString(testDataAttribute1.getName()).compareTo(safeString(testDataAttribute2.getName()));
		}

		/**
		 * @param testDataAttribute
		 * @return the rank for the given {@link TestDataAttribute}
		 */
		private int rank(TestDataAttribute testDataAttribute) {
			final MappingAttribute mappingAttribute = testDataAttribute.getMappingAttribute();

			if (mappingAttribute != null) {
				final DomainAttribute domainAttribute = mappingAttribute.getDomainAttribute();

				if (domainAttribute != null && mappingAttribute.getAssociation() == null) {
					// The primary key attribute has the highest priority!
					if (domainAttribute.isPk())
						return 0;

					if (domainAttribute.isDisplayAttribute())
						return 1;
				}
			}

			if (testDataAttribute.isMandatory())
				return 2;

			return 3;
		}

		/**
		 * Return an empty string if it is null
		 * @param string the string to be converted
		 * @return an empty string if the string is null or return the string
		 */
		private String safeString(String string) {
			return string == null ? "" : string;
		}
	}

}
