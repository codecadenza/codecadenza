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
package net.codecadenza.eclipse.testing.dialog.integration.test;

import net.codecadenza.eclipse.testing.dialog.AbstractDialog;
import net.codecadenza.eclipse.testing.domain.AssociationType;
import net.codecadenza.eclipse.testing.domain.DomainAssociation;
import net.codecadenza.eclipse.testing.domain.DomainAttribute;
import net.codecadenza.eclipse.testing.domain.DomainObject;
import net.codecadenza.eclipse.testing.domain.ElementCollectionType;
import net.codecadenza.eclipse.testing.domain.IntegrationBeanType;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Dialog for creating an integration test case
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateIntegrationTestCaseDialog extends AbstractDialog {
	private static final String CMD_ADD_OBJECT = "Add object";
	private static final String CMD_ADD_ITEM = "Add item";
	private static final String CMD_RANDOM = "Random";
	private static final String DEFAULT_USER = "test-user";
	private static final String DEFAULT_PASSWORD = "123456";
	private static final String ITEM_VALUE_PREFIX = "ITEM_";
	private static final String LBL_EXPECTED_SIZE = "Expected number of returned objects:";
	private static final String LBL_INTEGRATION_BEAN = "Integration bean:";
	private static final String LBL_INTEGRATION_METHOD = "Integration method:";
	private static final String LBL_NEW_ITEM_VALUE = "Value for a new element";
	private static final String LBL_OBJECT_ID_VALUE = "ID of new object";
	private static final String LBL_USER_NAME = "User name:";
	private static final String LBL_PASSWORD = "Password:";
	private static final String LBL_TIMEOUT = "Timeout:";
	private static final String METHOD_PREFIX_FIND = "find";
	private static final String METHOD_PREFIX_CREATE = "create";
	private static final String METHOD_PREFIX_UPDATE = "update";
	private static final String METHOD_PREFIX_SAVE = "save";
	private static final String METHOD_PREFIX_SEARCH = "searchAll";
	private static final String MNU_ADD_OBJECT = "Add object";
	private static final String OPERATOR_STARTS_WITH = "STARTS_WITH";
	private static final String OPERATOR_GREATER_OR_EQUAL = "GREATER_OR_EQUAL";
	private static final String OPERATOR_GREATER = "GREATER";
	private static final String TAB_ITEM_RETURN_VALUE = "Return value";
	private static final String SHELL_TITLE = "Create new integration test case";
	private static final String STRING_TEST_VALUE_SUFFIX = "_TEST_VALUE";
	private static final Logger log = LoggerFactory.getLogger(CreateIntegrationTestCaseDialog.class);

	private final DomainObject domainObject;
	private final IntegrationBeanType integrationBeanType;
	private final boolean addNestedInvocation;

	/**
	 * Constructor
	 * @param bot
	 * @param integrationBeanType
	 * @param domainObject
	 * @param addNestedInvocation
	 */
	public CreateIntegrationTestCaseDialog(SWTWorkbenchBot bot, IntegrationBeanType integrationBeanType, DomainObject domainObject,
			boolean addNestedInvocation) {
		super(bot, SHELL_TITLE);

		this.integrationBeanType = integrationBeanType;
		this.domainObject = domainObject;
		this.addNestedInvocation = addNestedInvocation;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.test.dialog.AbstractDialog#enterData()
	 */
	@Override
	public void enterData() {
		selectProposalItem(LBL_INTEGRATION_BEAN, domainObject.getIntegrationBeanName(integrationBeanType));

		final var cboMethod = bot.comboBoxWithLabel(LBL_INTEGRATION_METHOD);
		final int itemCount = cboMethod.itemCount();
		boolean withoutTimeout = false;

		// Iterate over all available integration methods and add them to the test
		for (int i = 0; i < itemCount; i++) {
			cboMethod.setSelection(i);
			bot.button(CMD_ADD).click();

			addInvocation(cboMethod.getText(), withoutTimeout);

			withoutTimeout = !withoutTimeout;
		}

		bot.button(CMD_OK).click();
	}

	/**
	 * Add a new integration test method invocation
	 * @param methodName the name of the method that is currently processed
	 * @param withoutTimeout flag that controls if the default timeout should be removed
	 */
	private void addInvocation(final String methodName, boolean withoutTimeout) {
		log.trace("Enter test data for invocation of method '{}'", methodName);
		enterTestData(methodName, true, withoutTimeout);

		bot.button(CMD_SAVE).click();

		if (addNestedInvocation)
			addNestedInvocation(methodName);
	}

	/**
	 * Add a nested invocation
	 * @param methodName the name of the method that is currently processed
	 */
	private void addNestedInvocation(final String methodName) {
		final var treeItems = bot.tree().getAllItems();

		for (final var treeItem : treeItems) {
			if (treeItem.getText().contains(methodName)) {
				treeItem.contextMenu(MNU_ADD).click();

				log.trace("Enter test data for nested invocation of method '{}'", methodName);
				enterTestData(methodName, false, false);
				break;
			}
		}

		bot.button(CMD_SAVE).click();
	}

	/**
	 * Enter some test data for a new integration test method invocation
	 * @param methodName the name of the method that is currently processed
	 * @param setAssertionOperators flag that controls if assertion operators should be set
	 * @param withoutTimeout flag that controls if the default timeout should be removed
	 */
	private void enterTestData(String methodName, boolean setAssertionOperators, boolean withoutTimeout) {
		boolean hasReturnValue = true;

		if (integrationBeanType == IntegrationBeanType.JMS || integrationBeanType == IntegrationBeanType.KAFKA) {
			// Asynchronous methods do not always have a return value!
			hasReturnValue = false;
		}
		else {
			bot.textWithLabel(LBL_USER_NAME).setText(DEFAULT_USER);
			bot.textWithLabel(LBL_PASSWORD).setText(DEFAULT_PASSWORD);
		}

		if (withoutTimeout)
			bot.textWithLabel(LBL_TIMEOUT).setText("");

		// Initialize fields with random values
		bot.button(CMD_RANDOM).click();

		// Make sure to just fill fields that really exist!
		if (methodName.startsWith(METHOD_PREFIX_CREATE) || methodName.startsWith(METHOD_PREFIX_SAVE)
				|| methodName.startsWith(METHOD_PREFIX_UPDATE)) {
			for (final DomainAttribute domainAttribute : domainObject.getAllAttributes()) {
				if (domainAttribute.getElementCollectionType() != ElementCollectionType.NONE)
					addItemsToElementCollection(domainAttribute);
				else if (domainAttribute.getType().equals(DomainAttribute.TYPE_STRING))
					addStringTestValue(domainAttribute, methodName);
			}

			for (final DomainAssociation assoc : domainObject.getAssociations())
				addTestObjectToAssociation(assoc);
		}
		else if (hasReturnValue && methodName.startsWith(METHOD_PREFIX_FIND) && methodName.contains("By")) {
			// Open the tab item that contains the return value
			bot.tabItem(TAB_ITEM_RETURN_VALUE).activate();

			if (setAssertionOperators)
				for (final DomainAttribute domainAttribute : domainObject.getAllAttributes())
					setAssertionOperators(domainAttribute, methodName);
		}
		else if (hasReturnValue && methodName.startsWith(METHOD_PREFIX_SEARCH))
			addObjectToSearchMethod(methodName);
	}

	/**
	 * Fill empty string test data input fields
	 * @param domainAttribute the domain attribute the test data attribute is mapped to
	 * @param methodName the name of the method that is currently processed
	 */
	private void addStringTestValue(DomainAttribute domainAttribute, String methodName) {
		final var txtInput = bot.textWithLabel(domainAttribute.getType() + " " + domainAttribute.getName());

		// Do not overwrite fields that have already been initialized!
		if (txtInput.getText().isEmpty()) {
			final var textToEnter = domainAttribute.getName().toUpperCase() + STRING_TEST_VALUE_SUFFIX;
			log.trace("Enter text for attribute '{}' in method '{}'", domainAttribute.getName(), methodName);

			txtInput.setText(textToEnter);
		}
	}

	/**
	 * Add some items to an element collection
	 * @param domainAttribute the domain attribute the element collection is mapped to
	 */
	private void addItemsToElementCollection(DomainAttribute domainAttribute) {
		final String tabItemName = "List<" + domainAttribute.getType() + "> " + domainAttribute.getName();

		// Open the tab item that contains the element collection
		bot.tabItem(tabItemName).activate();

		if (domainAttribute.getType().equals(DomainAttribute.TYPE_STRING)) {
			final var txtInput = bot.textWithLabel(LBL_NEW_ITEM_VALUE);

			for (int itemIndex = 1; itemIndex < 4; itemIndex++) {
				final String item = ITEM_VALUE_PREFIX + itemIndex;

				log.trace("Add item '{}' to element collection '{}'", item, domainAttribute.getName());

				txtInput.setText(item);
				bot.button(CMD_ADD_ITEM).click();
			}
		}
		else
			log.warn("No supported type for adding items to element collection '{}'!", domainAttribute.getName());
	}

	/**
	 * Add test data objects to a many-to-many association
	 * @param assoc the association
	 */
	private void addTestObjectToAssociation(DomainAssociation assoc) {
		if (assoc.getType() != AssociationType.MANY_TO_MANY)
			return;

		final String tabItemName = "List<" + assoc.getTarget().getName() + "ListDTO> " + assoc.getName();

		// Open the tab item that contains the many-to-many association
		bot.tabItem(tabItemName).activate();

		final var txtInput = bot.textWithLabel(LBL_OBJECT_ID_VALUE);
		final var pkAttr = assoc.getTarget().getAllAttributes().stream().filter(DomainAttribute::isPrimaryKey).findFirst()
				.orElseThrow();

		if (pkAttr.getType().equals(DomainAttribute.TYPE_LONG)) {
			for (int pkValue = 1; pkValue < 4; pkValue++) {
				log.trace("Add object with ID '{}' to many-to-many association '{}'", pkValue, assoc.getName());

				txtInput.setText(Integer.toString(pkValue));
				bot.button(CMD_ADD_OBJECT).click();
			}
		}
		else
			log.warn("No supported type for adding objects to many-to-many association '{}'!", assoc.getName());
	}

	/**
	 * Set the assertion operator for a given test data attribute
	 * @param domainAttribute the domain attribute the test data attribute is mapped to
	 * @param methodName the name of the method that is currently processed
	 */
	private void setAssertionOperators(DomainAttribute domainAttribute, String methodName) {
		final String attributeType = domainAttribute.getType();
		final String label = attributeType + " " + domainAttribute.getName();
		String operator = null;

		if (domainAttribute.getType().equals(DomainAttribute.TYPE_STRING)
				&& domainAttribute.getElementCollectionType() == ElementCollectionType.NONE)
			operator = OPERATOR_STARTS_WITH;
		else if (attributeType.equals(DomainAttribute.TYPE_DOUBLE) || attributeType.equals(DomainAttribute.TYPE_BIG_DECIMAL))
			operator = OPERATOR_GREATER_OR_EQUAL;

		if (operator != null) {
			log.trace("Select assertion operator for attribute '{}' of method '{}'", domainAttribute.getName(), methodName);

			bot.comboBoxWithLabel(label).setSelection(operator);
		}
	}

	/**
	 * Add a test data object to the result list of a search method
	 * @param methodName the name of the method that is currently processed
	 */
	private void addObjectToSearchMethod(String methodName) {
		bot.tabItem(TAB_ITEM_RETURN_VALUE).activate();

		final String testObjectType = domainObject.getName() + "SearchDTO";

		log.trace("Add test data object of type '{}' to method '{}'", testObjectType, methodName);

		bot.table().contextMenu(MNU_ADD_OBJECT).click();
		new EditTestObjectDialog(bot, testObjectType).enterData();

		bot.comboBoxWithLabel(LBL_EXPECTED_SIZE).setSelection(OPERATOR_GREATER);
		bot.textWithLabel(LBL_EXPECTED_SIZE).setText("1");
	}

}
