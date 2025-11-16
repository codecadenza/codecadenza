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
package net.codecadenza.eclipse.testing.view;

import static org.junit.jupiter.api.Assertions.fail;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import net.codecadenza.eclipse.testing.domain.DomainObject;
import net.codecadenza.eclipse.testing.domain.Project;
import org.eclipse.swtbot.eclipse.finder.SWTWorkbenchBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;

/**
 * <p>
 * Utility class for testing CodeCadenza's project view
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ProjectView extends AbstractView {
	private static final String MNU_DB_PROPERTIES = "Properties";
	private static final String MNU_DB_SYNC = "Synchronize";
	private static final String MNU_CREATE_TREE_VIEW = "Create tree view";
	private static final String MNU_CREATE_DATA_EXCHANGE_METHOD = "New data exchange method";
	private static final String MNU_CREATE_DEFAULT_FORMS = "Create default forms";
	private static final String MNU_CREATE_GRID_PANEL = "Create grid panel";
	private static final String MNU_CREATE_INTEGRATION_BEAN = "New integration bean";
	private static final String MNU_CREATE_TEST_CASE = "New test case";
	private static final String MNU_CREATE_UPDATE_FORM = "Create update form";
	private static final String MNU_CREATE_VIEW_FORM = "Create view form";
	private static final String MNU_EDIT_DATA_SOURCE = "Data source";
	private static final String MNU_EDIT_PROJECT = "Edit";
	private static final String MNU_PERSISTENCE_UNIT = "Persistence Unit";
	private static final String MNU_QUERY = "Query";
	private static final String MNU_REVERSE_ENGINEERING = "Reverse engineering";
	private static final String MNU_ROLES = "Roles";
	private static final String TREE_ITEM_BOUNDARY_BEANS = "Boundary beans";
	private static final String TREE_ITEM_REPOSITORIES = "Repositories";
	private static final String TREE_ITEM_DATABASE = "Database";
	private static final String TREE_ITEM_DATA_EXCHANGE = "Data exchange";
	private static final String TREE_ITEM_DTOS = "Data transfer objects";
	private static final String TREE_ITEM_FORM_GROUPS = "Form groups";
	private static final String TREE_ITEM_FORM_GROUP_MASTER = "Master data";
	private static final String TREE_ITEM_DOMAIN_OBJECTS = "Domain objects";
	private static final String TREE_ITEM_INTEGRATION_OBJECTS = "Integration objects";
	private static final String TREE_ITEM_SELENIUM = "Selenium";
	private static final String TREE_ITEM_TEST_MODULES = "Test modules";
	private static final String VIEW_TITLE = "CodeCadenza projects";

	private final Project project;

	/**
	 * Constructor
	 * @param bot
	 * @param project
	 */
	public ProjectView(SWTWorkbenchBot bot, Project project) {
		super(bot, VIEW_TITLE);

		this.project = project;
	}

	/**
	 * Click on the context menu item for editing the project
	 */
	public void clickContextMenuEditProject() {
		getProjectTreeItem().contextMenu(MNU_EDIT_PROJECT).click();
	}

	/**
	 * Click on the context menu item for editing the roles of a project
	 */
	public void clickContextMenuEditRoles() {
		getProjectTreeItem().contextMenu(MNU_ROLES).click();
	}

	/**
	 * Click on the context menu item for editing the persistence unit of a project
	 */
	public void clickContextMenuEditPersistenceUnit() {
		getProjectTreeItem().contextMenu(MNU_PERSISTENCE_UNIT).click();
	}

	/**
	 * Click on the context menu item for editing the project's data source
	 */
	public void clickContextMenuEditDataSource() {
		getProjectTreeItem().contextMenu(MNU_EDIT_DATA_SOURCE).click();
	}

	/**
	 * Click on the context menu item for performing the database synchronization
	 */
	public void clickContextMenuDBSync() {
		getProjectTreeItem().getNode(TREE_ITEM_DATABASE).contextMenu(MNU_DB_SYNC).click();
	}

	/**
	 * Click on the context menu item for performing reverse engineering
	 * @param namespace
	 */
	public void clickContextMenuReverseEngineering(String namespace) {
		getProjectTreeItem().getNode(TREE_ITEM_DOMAIN_OBJECTS).expand().getNode(namespace).expand()
				.contextMenu(MNU_REVERSE_ENGINEERING).click();
	}

	/**
	 * Click on the context menu item for editing the database properties
	 */
	public void clickContextMenuDBProperties() {
		getProjectTreeItem().getNode(TREE_ITEM_DATABASE).contextMenu(MNU_DB_PROPERTIES).click();
	}

	/**
	 * Open the SQL query editor for the given table
	 * @param tableName
	 */
	public void openTableInSQLEditor(String tableName) {
		getProjectTreeItem().getNode(TREE_ITEM_DATABASE).expand().getNode(tableName).expand().contextMenu(MNU_QUERY).click();
	}

	/**
	 * Open the source code of the given domain object in the editor
	 * @param domainObjectName
	 * @param namespace
	 */
	public void openDomainObjectInEditor(String domainObjectName, String namespace) {
		getProjectTreeItem().getNode(TREE_ITEM_DOMAIN_OBJECTS).expand().getNode(namespace).expand().getNode(domainObjectName)
				.doubleClick();
	}

	/**
	 * Get the domain object tree item
	 * @param domainObjectName
	 * @param namespace
	 * @return the domain object tree item
	 */
	public SWTBotTreeItem getDomainObjectTreeItem(String domainObjectName, String namespace) {
		return getProjectTreeItem().getNode(TREE_ITEM_DOMAIN_OBJECTS).expand().getNode(namespace).expand().getNode(domainObjectName)
				.expand();
	}

	/**
	 * Open the JPA query editor for the given domain object
	 * @param domainObjectName
	 * @param namespace
	 */
	public void openDomainObjectInQueryEditor(String domainObjectName, String namespace) {
		getProjectTreeItem().getNode(TREE_ITEM_DOMAIN_OBJECTS).expand().getNode(namespace).expand().getNode(domainObjectName)
				.contextMenu(MNU_QUERY).click();
	}

	/**
	 * Open the source code of the given enumeration in the editor
	 * @param enumName
	 * @param namespace
	 */
	public void openEnumerationInEditor(String enumName, String namespace) {
		openDomainObjectInEditor(enumName, namespace);
	}

	/**
	 * Click on the context menu item for creating default forms
	 */
	public void clickContextMenuDefaultForms() {
		getFormGroupMasterDataTreeItem().contextMenu(MNU_CREATE_DEFAULT_FORMS).click();
	}

	/**
	 * Click on the context menu item for creating an update form
	 */
	public void clickContextMenuCreateUpdateForm() {
		getFormGroupMasterDataTreeItem().contextMenu(MNU_CREATE_UPDATE_FORM).click();
	}

	/**
	 * Click on the context menu item for creating a view form
	 */
	public void clickContextMenuCreateViewForm() {
		getFormGroupMasterDataTreeItem().contextMenu(MNU_CREATE_VIEW_FORM).click();
	}

	/**
	 * Click on the context menu item for creating a grid panel
	 */
	public void clickContextMenuCreateGridPanel() {
		getFormGroupMasterDataTreeItem().contextMenu(MNU_CREATE_GRID_PANEL).click();
	}

	/**
	 * Click on the context menu item for creating a tree view
	 */
	public void clickContextMenuCreateTreeView() {
		getFormGroupMasterDataTreeItem().contextMenu(MNU_CREATE_TREE_VIEW).click();
	}

	/**
	 * Click on the context menu item for deleting a form
	 * @param formName
	 */
	public void clickContextMenuDeleteForm(String formName) {
		getFormGroupMasterDataTreeItem().getNode(formName).contextMenu(MNU_DELETE).click();
	}

	/**
	 * Click on the context menu item for editing a form
	 * @param formName
	 */
	public void clickContextMenuEditForm(String formName) {
		getFormGroupMasterDataTreeItem().getNode(formName).contextMenu(MNU_EDIT).click();
	}

	/**
	 * Click on the context menu item for renaming a form
	 * @param formName
	 */
	public void clickContextMenuRenameForm(String formName) {
		getFormGroupMasterDataTreeItem().getNode(formName).contextMenu(MNU_RENAME).click();
	}

	/**
	 * Open the source code of the given form in the editor
	 * @param formName
	 */
	public void openFormInEditor(String formName) {
		final var formGroupMasterDataTreeItem = getFormGroupMasterDataTreeItem().expand();

		for (final var formTreeItem : formGroupMasterDataTreeItem.getItems())
			if (formTreeItem.getText().equals(formName)) {
				formTreeItem.doubleClick();
				return;
			}

		fail("The form '" + formName + "' could not be found!");
	}

	/**
	 * @return the names of all existing forms that belong to the 'Master data' form group
	 */
	public List<String> getFormNames() {
		final var formTreeItems = new ArrayList<String>();

		for (final var formItem : getFormGroupMasterDataTreeItem().expand().getItems())
			formTreeItems.add(formItem.getText());

		return formTreeItems;
	}

	/**
	 * Click on the context menu item for adding a new form group to the 'Master data' form group
	 */
	public void clickContextMenuAddFormGroup() {
		getFormGroupMasterDataTreeItem().contextMenu(MNU_ADD).click();
	}

	/**
	 * Click on the context menu item for editing an existing form group
	 * @param formGroupName
	 */
	public void clickContextMenuEditFormGroup(String formGroupName) {
		for (final var formGroupTreeItem : getFormGroupMasterDataTreeItem().getItems())
			if (formGroupTreeItem.getText().equals(formGroupName)) {
				formGroupTreeItem.contextMenu(MNU_EDIT).click();
				return;
			}

		fail("The form group '" + formGroupName + "' could not be found!");
	}

	/**
	 * Click on the context menu item for deleting a form group
	 * @param formGroupName
	 */
	public void clickContextMenuDeleteFormGroup(String formGroupName) {
		for (final var formGroupTreeItem : getFormGroupMasterDataTreeItem().getItems())
			if (formGroupTreeItem.getText().equals(formGroupName)) {
				formGroupTreeItem.contextMenu(MNU_DELETE).click();
				return;
			}

		fail("The form group '" + formGroupName + "' could not be found!");
	}

	/**
	 * Click on the context menu item for editing a boundary bean
	 * @param boundaryBeanName
	 * @param namespace
	 */
	public void clickContextMenuEditBoundaryBean(String boundaryBeanName, String namespace) {
		getProjectTreeItem().getNode(TREE_ITEM_BOUNDARY_BEANS).expand().getNode(namespace).expand().getNode(boundaryBeanName)
				.contextMenu(MNU_EDIT).click();
	}

	/**
	 * Click on the context menu item for deleting a boundary bean
	 * @param boundaryBeanName
	 * @param namespace
	 */
	public void clickContextMenuDeleteBoundaryBean(String boundaryBeanName, String namespace) {
		getProjectTreeItem().getNode(TREE_ITEM_BOUNDARY_BEANS).expand().getNode(namespace).expand().getNode(boundaryBeanName)
				.contextMenu(MNU_DELETE).click();
	}

	/**
	 * Open the source code of the given boundary bean in the editor
	 * @param boundaryBeanName
	 * @param namespace
	 */
	public void openBoundaryBeanInEditor(String boundaryBeanName, String namespace) {
		final var boundaryPackageTreeItem = getProjectTreeItem().getNode(TREE_ITEM_BOUNDARY_BEANS).expand().getNode(namespace)
				.expand();

		for (final var boundaryTreeItem : boundaryPackageTreeItem.getItems())
			if (boundaryTreeItem.getText().equals(boundaryBeanName)) {
				boundaryTreeItem.doubleClick();
				return;
			}

		fail("The boundary bean '" + boundaryBeanName + "' could not be found!");
	}

	/**
	 * Click on the context menu item for deleting a repository
	 * @param repositoryName
	 * @param namespace
	 */
	public void clickContextMenuDeleteRepository(String repositoryName, String namespace) {
		getProjectTreeItem().getNode(TREE_ITEM_REPOSITORIES).expand().getNode(namespace).expand().getNode(repositoryName)
				.contextMenu(MNU_DELETE).click();
	}

	/**
	 * Click on the context menu item for renaming a repository
	 * @param repositoryName
	 * @param namespace
	 */
	public void clickContextMenuRenameRepository(String repositoryName, String namespace) {
		getProjectTreeItem().getNode(TREE_ITEM_REPOSITORIES).expand().getNode(namespace).expand().getNode(repositoryName)
				.contextMenu(MNU_RENAME).click();
	}

	/**
	 * Open the source code of the given repository in the editor
	 * @param repositoryName
	 * @param namespace
	 */
	public void openRepositoryInEditor(String repositoryName, String namespace) {
		final var repositoryPackageTreeItem = getProjectTreeItem().getNode(TREE_ITEM_REPOSITORIES).expand().getNode(namespace)
				.expand();

		for (final var repositoryItem : repositoryPackageTreeItem.getItems())
			if (repositoryItem.getText().equals(repositoryName)) {
				repositoryItem.doubleClick();
				return;
			}

		fail("The repository '" + repositoryName + "' could not be found!");
	}

	/**
	 * Click on the context menu item for deleting a DTO
	 * @param dtoName
	 * @param namespace
	 */
	public void clickContextMenuDeleteDTO(String dtoName, String namespace) {
		getProjectTreeItem().getNode(TREE_ITEM_DTOS).expand().getNode(namespace).expand().getNode(dtoName).contextMenu(MNU_DELETE)
				.click();
	}

	/**
	 * Click on the context menu item for creating a new GUI test
	 */
	public void clickContextMenuCreateGUITest() {
		getProjectTreeItem().getNode(TREE_ITEM_TEST_MODULES).expand().getNode(TREE_ITEM_SELENIUM).click()
				.contextMenu(MNU_CREATE_TEST_CASE).click();
	}

	/**
	 * Click on the context menu item for editing a GUI test
	 * @param testCaseName
	 */
	public void clickContextMenuEditGUITest(String testCaseName) {
		getProjectTreeItem().getNode(TREE_ITEM_TEST_MODULES).expand().getNode(TREE_ITEM_SELENIUM).expand().getNode(testCaseName)
				.contextMenu(MNU_EDIT).click();
	}

	/**
	 * Open the source code of the given GUI test in the editor
	 * @param testCaseName
	 */
	public void openGUITestInEditor(String testCaseName) {
		final var seleniumTreeItem = getProjectTreeItem().getNode(TREE_ITEM_TEST_MODULES).expand().getNode(TREE_ITEM_SELENIUM)
				.expand();

		for (final var testCaseTreeItem : seleniumTreeItem.getItems())
			if (testCaseTreeItem.getText().equals(testCaseName)) {
				testCaseTreeItem.doubleClick();
				return;
			}

		fail("The GUI test '" + testCaseName + "' could not be found!");
	}

	/**
	 * Click on the context menu item for deleting a GUI test
	 * @param testCaseName
	 */
	public void clickContextMenuDeleteGUITest(String testCaseName) {
		getProjectTreeItem().getNode(TREE_ITEM_TEST_MODULES).expand().getNode(TREE_ITEM_SELENIUM).expand().getNode(testCaseName)
				.contextMenu(MNU_DELETE).click();
	}

	/**
	 * @return all existing data transfer objects
	 */
	public List<String> getDTOTreeNodes() {
		final var dtoNamespaceItems = getProjectTreeItem().getNode(TREE_ITEM_DTOS).expand().getItems();
		final var dtoList = new ArrayList<String>();

		for (final var dtoNamespaceItem : dtoNamespaceItems) {
			dtoNamespaceItem.expand();
			dtoList.addAll(dtoNamespaceItem.getNodes());
		}

		return dtoList;
	}

	/**
	 * Open the source code of the given DTO in the editor
	 * @param dtoName
	 * @param namespace
	 */
	public void openDTOInEditor(String dtoName, String namespace) {
		final var dtoPackageTreeItem = getProjectTreeItem().getNode(TREE_ITEM_DTOS).expand().getNode(namespace).expand();

		for (final var dtoTreeItem : dtoPackageTreeItem.getItems())
			if (dtoTreeItem.getText().equals(dtoName)) {
				dtoTreeItem.doubleClick();
				return;
			}

		fail("The data transfer object '" + dtoName + "' could not be found!");
	}

	/**
	 * Click on the context menu item for editing the given DTO
	 * @param dtoName
	 * @param namespace
	 */
	public void clickContextMenuEditDTO(String dtoName, String namespace) {
		final var dtoPackageTreeItem = getProjectTreeItem().getNode(TREE_ITEM_DTOS).expand().getNode(namespace).expand();

		for (final var dtoTreeItem : dtoPackageTreeItem.getItems())
			if (dtoTreeItem.getText().equals(dtoName)) {
				dtoTreeItem.contextMenu(MNU_EDIT).click();
				return;
			}

		fail("The data transfer object '" + dtoName + "' could not be found!");
	}

	/**
	 * Click on the context menu item for creating a data exchange method
	 */
	public void clickContextMenuCreateExchangeMethod() {
		getProjectTreeItem().getNode(TREE_ITEM_DATA_EXCHANGE).contextMenu(MNU_CREATE_DATA_EXCHANGE_METHOD).click();
	}

	/**
	 * Open the source code of the given data exchange bean in the editor
	 * @param domainObject
	 */
	public void openDataExchangeBeanInEditor(DomainObject domainObject) {
		final var namespace = domainObject.getNamespace();
		final var projectTreeItem = getProjectTreeItem();
		final var dataExchangeTreeItem = projectTreeItem.getNode(TREE_ITEM_DATA_EXCHANGE).expand().getNode(namespace).expand();

		for (final var dataExchangeBeanTreeItem : dataExchangeTreeItem.getItems())
			if (dataExchangeBeanTreeItem.getText().equals(domainObject.getDataExchangeServiceName())) {
				dataExchangeBeanTreeItem.doubleClick();
				return;
			}

		fail("The data exchange bean '" + domainObject.getDataExchangeServiceName() + "' could not be found!");
	}

	/**
	 * Click on the context menu item for deleting a data exchange bean
	 * @param domainObject
	 */
	public void clickContextMenuDeleteDataExchangeBean(DomainObject domainObject) {
		getProjectTreeItem().getNode(TREE_ITEM_DATA_EXCHANGE).expand().getNode(domainObject.getNamespace()).expand()
				.getNode(domainObject.getDataExchangeServiceName()).contextMenu(MNU_DELETE).click();
	}

	/**
	 * Click on the context menu item for editing a data exchange bean
	 * @param domainObject
	 */
	public void clickContextMenuEditDataExchangeBean(DomainObject domainObject) {
		getProjectTreeItem().getNode(TREE_ITEM_DATA_EXCHANGE).expand().getNode(domainObject.getNamespace()).expand()
				.getNode(domainObject.getDataExchangeServiceName()).expand().getNode(0).contextMenu(MNU_EDIT).click();
	}

	/**
	 * Click on the context menu item for creating an integration object
	 * @param packageTreeItem
	 */
	public void clickContextMenuCreateIntegrationBean(SWTBotTreeItem packageTreeItem) {
		packageTreeItem.contextMenu(MNU_CREATE_INTEGRATION_BEAN).click();
	}

	/**
	 * Open the source code of the given integration bean in the editor
	 * @param integrationBeanName
	 */
	public void openIntegrationBeanInEditor(String integrationBeanName) {
		for (final var integrationPackageTreeItem : getIntegrationBeansTreeItem().getItems()) {
			integrationPackageTreeItem.expand();

			for (final var integrationBeanTreeItem : integrationPackageTreeItem.getItems())
				if (integrationBeanTreeItem.getText().equals(integrationBeanName)) {
					integrationBeanTreeItem.doubleClick();
					return;
				}
		}

		fail("The integration bean '" + integrationBeanName + "' could not be found!");
	}

	/**
	 * @return the integration bean tree item
	 */
	public SWTBotTreeItem getIntegrationBeansTreeItem() {
		return getProjectTreeItem().getNode(TREE_ITEM_INTEGRATION_OBJECTS).expand();
	}

	/**
	 * @return the tree item that contains all test modules
	 */
	public SWTBotTreeItem getTestModulesTreeItem() {
		return getProjectTreeItem().getNode(TREE_ITEM_TEST_MODULES).expand();
	}

	/**
	 * @return all integration test module names of the current project
	 */
	public List<String> getIntegrationTestModuleNames() {
		return Arrays.asList(getTestModulesTreeItem().getItems()).stream().map(SWTBotTreeItem::getText)
				.filter(i -> !i.equals(TREE_ITEM_SELENIUM)).toList();
	}

	/**
	 * Click on the context menu item for creating an integration test case
	 * @param moduleTreeItem
	 */
	public void clickContextMenuCreateIntegrationTest(SWTBotTreeItem moduleTreeItem) {
		moduleTreeItem.contextMenu(MNU_CREATE_TEST_CASE).click();
	}

	/**
	 * Open the source code of the given integration test case in the editor
	 * @param moduleName
	 * @param integrationTestCaseName
	 */
	public void openIntegrationTestCaseInEditor(String moduleName, String integrationTestCaseName) {
		for (final var integrationTestCaseItem : getTestModulesTreeItem().expand().getNode(moduleName).expand().getItems())
			if (integrationTestCaseItem.getText().equals(integrationTestCaseName)) {
				integrationTestCaseItem.doubleClick();
				return;
			}

		fail("The integration test case '" + integrationTestCaseName + "' could not be found!");
	}

	/**
	 * @return the form group 'Master data' tree node
	 */
	private SWTBotTreeItem getFormGroupMasterDataTreeItem() {
		return getProjectTreeItem().getNode(TREE_ITEM_FORM_GROUPS).expand().getNode(TREE_ITEM_FORM_GROUP_MASTER).select();
	}

	/**
	 * Refresh the view and get the root item of the given project
	 * @return the project's root item
	 */
	private SWTBotTreeItem getProjectTreeItem() {
		final var projectView = bot.viewByTitle(VIEW_TITLE);
		projectView.setFocus();

		// Refresh the project view
		projectView.getToolbarButtons().get(0).click();

		return projectView.bot().tree().getTreeItem(project.getName()).select().expand();
	}

}
