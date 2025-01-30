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

import java.util.LinkedList;
import java.util.ListIterator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.testing.GUITestCase;
import net.codecadenza.eclipse.model.testing.TestSuite;
import net.codecadenza.eclipse.service.repository.RepositoryService;
import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.swt.widgets.TreeItem;

/**
 * <p>
 * Helper class to restore the selection of the project tree view
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TreeSelectionHelper {
	private JavaType type;
	private Namespace namespace;
	private FormGroup group;
	private final ProjectView view;
	private final Project project;
	private JavaMethod method;
	private DomainAttribute attribute;
	private EnumLiteral literal;
	private DTOBeanAttribute dtoAttribute;
	private Database database;
	private DBTable table;
	private AbstractDomainAssociation assoc;
	private Form form;
	private FormPanel formPanel;
	private DBColumn column;
	private String itemText;

	/**
	 * @param view
	 * @param project
	 * @param itemText
	 */
	public TreeSelectionHelper(ProjectView view, Project project, String itemText) {
		this.view = view;
		this.project = project;
		this.itemText = itemText;
	}

	/**
	 * @param view
	 * @param type
	 */
	public TreeSelectionHelper(ProjectView view, JavaType type) {
		this.type = type;
		this.namespace = type.getNamespace();
		this.view = view;
		this.project = namespace.getProject();
	}

	/**
	 * @param view
	 * @param namespace
	 */
	public TreeSelectionHelper(ProjectView view, Namespace namespace) {
		this.namespace = namespace;
		this.view = view;
		this.project = namespace.getProject();
	}

	/**
	 * @param view
	 * @param database
	 */
	public TreeSelectionHelper(ProjectView view, Database database) {
		this.database = database;
		this.view = view;
		this.project = database.getProject();
	}

	/**
	 * @param view
	 * @param table
	 */
	public TreeSelectionHelper(ProjectView view, DBTable table) {
		this.database = table.getDatabase();
		this.table = table;
		this.view = view;
		this.project = database.getProject();
	}

	/**
	 * @param view
	 * @param column
	 */
	public TreeSelectionHelper(ProjectView view, DBColumn column) {
		this.column = column;
		this.database = column.getDatabaseTable().getDatabase();
		this.table = column.getDatabaseTable();
		this.view = view;
		this.project = database.getProject();
	}

	/**
	 * @param view
	 * @param attribute
	 */
	public TreeSelectionHelper(ProjectView view, DomainAttribute attribute) {
		this(view, attribute.getDomainObject());

		this.attribute = attribute;
	}

	/**
	 * @param view
	 * @param assoc
	 */
	public TreeSelectionHelper(ProjectView view, AbstractDomainAssociation assoc) {
		this(view, assoc.getDomainObject());

		this.assoc = assoc;
	}

	/**
	 * @param view
	 * @param dtoAttribute
	 */
	public TreeSelectionHelper(ProjectView view, DTOBeanAttribute dtoAttribute) {
		this(view, dtoAttribute.getDTOBean());

		this.dtoAttribute = dtoAttribute;
	}

	/**
	 * @param view
	 * @param literal
	 */
	public TreeSelectionHelper(ProjectView view, EnumLiteral literal) {
		this(view, literal.getJavaEnum());

		this.literal = literal;
	}

	/**
	 * @param view
	 * @param method
	 */
	public TreeSelectionHelper(ProjectView view, JavaMethod method) {
		this(view, method.getJavaType());

		this.method = method;
	}

	/**
	 * @param view
	 * @param method
	 */
	public TreeSelectionHelper(ProjectView view, BoundaryMethod method) {
		this(view, method.getBoundaryBean());

		this.method = method;
	}

	/**
	 * @param view
	 * @param method
	 */
	public TreeSelectionHelper(ProjectView view, DataExchangeMethod method) {
		this(view, method.getDataExchangeServiceBean());

		this.method = method;
	}

	/**
	 * @param view
	 * @param method
	 */
	public TreeSelectionHelper(ProjectView view, AbstractIntegrationMethod method) {
		this(view, method.getIntegrationBean());

		this.method = method;
	}

	/**
	 * @param view
	 * @param boundary
	 * @param method
	 */
	public TreeSelectionHelper(ProjectView view, BoundaryBean boundary, RepositoryMethod method) {
		this(view, boundary);

		this.method = method;
	}

	/**
	 * @param view
	 * @param form
	 */
	public TreeSelectionHelper(ProjectView view, Form form) {
		this.group = form.getFormGroup();
		this.view = view;
		this.form = form;
		this.project = group.findProject();
	}

	/**
	 * @param view
	 * @param panel
	 */
	public TreeSelectionHelper(ProjectView view, FormPanel panel) {
		this.group = panel.getFormGroup();
		this.view = view;
		this.formPanel = panel;
		this.project = group.findProject();
	}

	/**
	 * @param view
	 * @param group
	 */
	public TreeSelectionHelper(ProjectView view, FormGroup group) {
		this.group = group;
		this.view = view;
		this.project = group.findProject();
	}

	/**
	 * @param project
	 * @return true if the selection is part of the given project
	 */
	public boolean belongsToProject(Project project) {
		return this.project.getName().equals(project.getName());
	}

	/**
	 * @param projectItem
	 * @param label
	 * @return the node that has been expanded
	 */
	private TreeItem expandNamespace(TreeItem projectItem, String label) {
		for (final TreeItem i : projectItem.getItems())
			if (i.getText().equals(label)) {
				view.addNamespaceItems(i);
				i.setExpanded(true);

				for (final TreeItem j : i.getItems())
					if (j.getText().equals(namespace.getName())) {
						view.addNamespaceItems(j);
						j.setExpanded(true);
						return j;
					}
			}

		return null;
	}

	/**
	 * @param parentItem
	 * @param groupList
	 * @return a tree item
	 */
	private TreeItem getFormGroupsToBeExpanded(TreeItem parentItem, LinkedList<TreeItem> groupList) {
		for (final TreeItem treeItem : parentItem.getItems()) {
			// We are only interested in form groups!
			if (!(treeItem.getData() instanceof final FormGroup currentGroup))
				continue;

			// The group name and name of the parent group must match!
			if (currentGroup.getName().equals(group.getName())) {
				if (currentGroup.getParentGroup() == null) {
					if (group.getParentGroup() == null)
						return treeItem;
				}
				else if (group.getParentGroup() != null
						&& group.getParentGroup().getName().equals(currentGroup.getParentGroup().getName()))
					return treeItem;
			}

			// We must go down the tree
			final TreeItem f = getFormGroupsToBeExpanded(treeItem, groupList);

			if (f != null) {
				// We must save the group within the linked list as we have to expand it afterwards!
				groupList.addFirst(f);
				return treeItem;
			}
		}

		return null;
	}

	/**
	 * @param parentItem
	 */
	private void expandBean(TreeItem parentItem) {
		for (final TreeItem i : parentItem.getItems()) {
			if (i.getData() instanceof final BoundaryBean boundary && boundary.getName().equals(type.getName())) {
				view.addBoundaryChildItems(i, boundary);
				i.setExpanded(true);

				for (final TreeItem k : i.getItems()) {
					if (k.getData() instanceof final BoundaryMethod boundaryMethod && boundaryMethod.getName().equals(method.getName())) {
						view.getTree().select(k);
						return;
					}

					if (!project.isBoundaryMode() && k.getData() instanceof final RepositoryMethod repositoryMethod) {
						final var repositoryService = new RepositoryService(project);

						// We must compare signatures because there are some overloaded methods!
						final String signature1 = repositoryService.createSimpleSignature(repositoryMethod);
						var signature2 = "";

						// If this method isn't a repository method (e.g. a facade method) the respective tree item won't be found!
						if (method instanceof final RepositoryMethod aMethod)
							signature2 = repositoryService.createSimpleSignature(aMethod);

						if (signature1.equals(signature2)) {
							view.getTree().select(k);
							return;
						}
					}
				}

				view.getTree().select(i);
				return;
			}
			else if (i.getData() instanceof final DataExchangeServiceBean exchangeBean
					&& exchangeBean.getName().equals(type.getName())) {
				view.addDataExchangeServiceChildItems(i, exchangeBean);
				i.setExpanded(true);

				for (final TreeItem k : i.getItems()) {
					if (k.getData() instanceof final DataExchangeMethod exchangeMethod
							&& exchangeMethod.getName().equals(method.getName())) {
						view.getTree().select(k);
						return;
					}
				}

				view.getTree().select(i);
				return;
			}
			else if (i.getData() instanceof final Repository repository && repository.getName().equals(type.getName())) {
				view.addRepositoryChildItems(i, repository);
				i.setExpanded(true);

				for (final TreeItem k : i.getItems()) {
					if (k.getData() instanceof final BoundaryMethod boundaryMethod && boundaryMethod.getName().equals(method.getName())) {
						view.getTree().select(k);
						return;
					}

					final var repositoryService = new RepositoryService(project);
					final var repositoryMethod = (RepositoryMethod) k.getData();

					// We must compare the signatures because there are some overloaded methods!
					final String signature1 = repositoryService.createSimpleSignature(repositoryMethod);
					var signature2 = "";

					// If this method isn't a repository method the respective tree item won't be found!
					if (method instanceof final RepositoryMethod aMethod)
						signature2 = repositoryService.createSimpleSignature(aMethod);

					if (signature1.equals(signature2)) {
						view.getTree().select(k);
						return;
					}
				}

				view.getTree().select(i);
				return;
			}
			else if (i.getData() instanceof final DomainObject domainObject && type instanceof DomainObject
					&& domainObject.getName().equals(type.getName())) {
				view.addDomainObjectChildItems(i, domainObject);
				i.setExpanded(true);

				for (final TreeItem k : i.getItems()) {
					var name = "";

					if (k.getData() instanceof final AbstractDomainAssociation domainAssoc)
						name = domainAssoc.getName();
					else
						name = ((DomainAttribute) k.getData()).getName();

					if (attribute != null && name.equals(attribute.getName())) {
						view.getTree().select(k);
						return;
					}

					if (assoc != null && name.equals(assoc.getName())) {
						view.getTree().select(k);
						return;
					}
				}

				view.getTree().select(i);
				return;
			}
			else if (i.getData() instanceof final JavaEnum enumeration && type instanceof JavaEnum
					&& enumeration.getName().equals(type.getName())) {
				view.addJavaEnumChildItems(i, enumeration);
				i.setExpanded(true);

				for (final TreeItem k : i.getItems()) {
					final var enumLiteral = (EnumLiteral) k.getData();

					if (enumLiteral.getName().equals(literal.getName())) {
						view.getTree().select(k);
						return;
					}
				}

				view.getTree().select(i);
				return;
			}
			else if (i.getData() instanceof final DTOBean dto && dto.getName().equals(type.getName())) {
				view.addDTOChildItems(i, dto);
				i.setExpanded(true);

				for (final TreeItem k : i.getItems()) {
					final var attr = (DTOBeanAttribute) k.getData();

					if (attr.getName().equals(dtoAttribute.getName())) {
						view.getTree().select(k);
						return;
					}
				}

				view.getTree().select(i);
				return;
			}
			else if (i.getData() instanceof final AbstractIntegrationBean integrationBean
					&& integrationBean.getName().equals(type.getName())) {
				view.addIntegrationBeanChildItems(i, integrationBean);
				i.setExpanded(true);

				for (final TreeItem k : i.getItems()) {
					if (k.getData() instanceof final AbstractIntegrationMethod integrationMethod
							&& integrationMethod.getName().equals(method.getName())) {
						view.getTree().select(k);
						return;
					}
				}

				view.getTree().select(i);
				return;
			}
		}
	}

	/**
	 * @param projectItem
	 */
	public void expandTree(TreeItem projectItem) {
		view.addProjectItems(projectItem);
		projectItem.setExpanded(true);
		TreeItem expandedItem = null;

		if (type != null) {
			if (type instanceof DTOBean) {
				expandedItem = expandNamespace(projectItem, ProjectView.DTO_NAMESPACE_LABEL);

				if (dtoAttribute != null && expandedItem != null)
					expandBean(expandedItem);

				if (dtoAttribute == null && expandedItem != null) {
					for (final TreeItem k : expandedItem.getItems())
						if (k.getText().equals(type.getName())) {
							view.getTree().select(k);
							return;
						}
				}
			}
			else if (type instanceof ExchangeMappingObject) {
				expandedItem = expandNamespace(projectItem, ProjectView.DATA_EXCHANGE_NAMESPACE_LABEL);

				if (expandedItem != null)
					expandBean(expandedItem);

				if (expandedItem != null) {
					for (final TreeItem k : expandedItem.getItems())
						if (k.getText().equals(type.getName())) {
							view.getTree().select(k);
							return;
						}
				}
			}
			else if (type instanceof Repository) {
				expandedItem = expandNamespace(projectItem, ProjectView.REPOSITORY_NAMESPACE_LABEL);

				if (method != null && expandedItem != null)
					expandBean(expandedItem);

				if (method == null && expandedItem != null) {
					for (final TreeItem k : expandedItem.getItems()) {
						if (!(k.getData() instanceof final Repository repository))
							continue;

						if (repository.getName().equals(type.getName())) {
							view.getTree().select(k);
							return;
						}
					}
				}
			}
			else if (type instanceof BoundaryBean) {
				if (namespace.getProject().isBoundaryMode())
					expandedItem = expandNamespace(projectItem, ProjectView.BOUNDARY_NAMESPACE_LABEL);
				else
					expandedItem = expandNamespace(projectItem, ProjectView.FACADE_NAMESPACE_LABEL);

				if (method != null && expandedItem != null)
					expandBean(expandedItem);

				if (method == null && expandedItem != null) {
					for (final TreeItem k : expandedItem.getItems()) {
						if (!(k.getData() instanceof final BoundaryBean boundary))
							continue;

						if (boundary.getName().equals(type.getName())) {
							view.getTree().select(k);
							return;
						}
					}
				}
			}
			else if (type instanceof DomainObject) {
				expandedItem = expandNamespace(projectItem, ProjectView.DOMAIN_NAMESPACE_LABEL);

				if ((attribute != null || assoc != null) && expandedItem != null)
					expandBean(expandedItem);

				if (attribute == null && assoc == null && expandedItem != null) {
					for (final TreeItem k : expandedItem.getItems()) {
						if (!(k.getData() instanceof final DomainObject domainObject))
							continue;

						if (domainObject.getName().equals(type.getName())) {
							view.getTree().select(k);
							return;
						}
					}
				}
			}
			else if (type instanceof JavaEnum) {
				expandedItem = expandNamespace(projectItem, ProjectView.DOMAIN_NAMESPACE_LABEL);

				if (literal != null && expandedItem != null)
					expandBean(expandedItem);

				if (literal == null && expandedItem != null) {
					for (final TreeItem k : expandedItem.getItems()) {
						if (!(k.getData() instanceof final JavaEnum javaEnum))
							continue;

						if (javaEnum.getName().equals(type.getName())) {
							view.getTree().select(k);
							return;
						}
					}
				}
			}
			else if (type instanceof DataExchangeServiceBean) {
				expandedItem = expandNamespace(projectItem, ProjectView.DATA_EXCHANGE_NAMESPACE_LABEL);

				if (method != null && expandedItem != null)
					expandBean(expandedItem);

				if (method == null && expandedItem != null) {
					for (final TreeItem k : expandedItem.getItems()) {
						if (!(k.getData() instanceof final DataExchangeServiceBean exchangeBean))
							continue;

						if (exchangeBean.getName().equals(type.getName())) {
							view.getTree().select(k);
							return;
						}
					}
				}
			}
			else if (type instanceof AbstractIntegrationBean) {
				TreeItem currentItem = null;

				for (final TreeItem k : projectItem.getItems())
					if (k.getText().equals(ProjectView.INTEGRATION_LABEL)) {
						currentItem = k;
						k.setExpanded(true);
						view.getTree().select(k);
						break;
					}

				if (currentItem != null)
					for (final TreeItem k : currentItem.getItems()) {
						final var itemNamespace = (Namespace) k.getData();

						if (type.getNamespace() != null && itemNamespace.toString().equals(type.getNamespace().toString())) {
							expandedItem = k;
							view.addNamespaceItems(k);
							k.setExpanded(true);
							view.getTree().select(k);
							break;
						}
					}

				if (method != null && expandedItem != null)
					expandBean(expandedItem);

				if (method == null && expandedItem != null) {
					for (final TreeItem k : expandedItem.getItems()) {
						if (!(k.getData() instanceof final AbstractIntegrationBean integrationBean))
							continue;

						if (integrationBean.getName().equals(type.getName())) {
							view.getTree().select(k);
							return;
						}
					}
				}
			}
			else if (type instanceof GUITestCase || type instanceof TestSuite) {
				TreeItem currentItem = null;

				for (final TreeItem k : projectItem.getItems())
					if (k.getText().equals(ProjectView.TEST_MODULES_LABEL)) {
						currentItem = k;
						k.setExpanded(true);
						view.getTree().select(k);
						break;
					}

				if (currentItem != null)
					for (final TreeItem k : currentItem.getItems()) {
						final var itemNamespace = (Namespace) k.getData();

						if (itemNamespace.toString().equals(type.getNamespace().toString())) {
							expandedItem = k;
							view.addNamespaceItems(k);
							k.setExpanded(true);
							view.getTree().select(k);
							break;
						}
					}

				if (expandedItem != null) {
					for (final TreeItem k : expandedItem.getItems()) {
						if (!(k.getData() instanceof final JavaType javaType))
							continue;

						if (javaType.getName().equals(type.getName())) {
							view.getTree().select(k);
							return;
						}
					}
				}
			}

			return;
		}

		// Expand the namespace tree item
		if (database == null && namespace != null) {
			for (final TreeItem i : projectItem.getItems()) {
				if (!i.getParentItem().getText().equals(project.getName()))
					continue;

				if (i.getData() == null)
					if (i.getText().equals(ProjectView.INTEGRATION_LABEL)) {
						final var proj = (Project) projectItem.getData();

						if (proj.getIntegrationModules().stream().anyMatch(e -> e.getNamespace().toString().equals(namespace.toString()))) {
							i.setExpanded(true);

							for (final TreeItem k : i.getItems()) {
								final var ns = (Namespace) k.getData();

								if (ns.toString().equals(namespace.toString())) {
									view.getTree().select(k);
									return;
								}
							}
						}
					}
					else if (i.getText().equals(ProjectView.TEST_MODULES_LABEL)) {
						final var proj = (Project) projectItem.getData();

						if (proj.getTestModules().stream().anyMatch(e -> e.getNamespace().toString().equals(namespace.toString()))) {
							i.setExpanded(true);

							for (final TreeItem k : i.getItems()) {
								final var ns = (Namespace) k.getData();

								if (ns.toString().equals(namespace.toString())) {
									view.getTree().select(k);
									return;
								}
							}
						}
					}

				if (!(i.getData() instanceof final Namespace treeNamespace))
					continue;

				if (treeNamespace.toString().equals(namespace.getParent().toString())) {
					view.addNamespaceItems(i);
					i.setExpanded(true);

					for (final TreeItem k : i.getItems()) {
						if (!(k.getData() instanceof final Namespace ns))
							continue;

						if (ns.toString().equals(namespace.toString())) {
							view.getTree().select(k);
							return;
						}
					}

					return;
				}

				if (treeNamespace.toString().equals(namespace.toString())) {
					view.getTree().select(i);
					return;
				}
			}

			return;
		}

		// Expand the database tree item
		if (database != null && table != null && namespace == null) {
			for (final TreeItem i : projectItem.getItems()) {
				if (!(i.getData() instanceof Database))
					continue;

				view.addDatabaseItems(i);
				i.setExpanded(true);

				if (column == null) {
					for (final TreeItem k : i.getItems()) {
						final var t = (DBTable) k.getData();

						if (t.getFullDatabaseName().equals(table.getFullDatabaseName())) {
							view.getTree().select(k);
							return;
						}
					}
				}
				else {
					for (final TreeItem k : i.getItems()) {
						final var t = (DBTable) k.getData();

						if (t.getFullDatabaseName().equals(table.getFullDatabaseName())) {
							view.addTableItems(k);
							k.setExpanded(true);

							// Restore the previous column selection
							for (final TreeItem m : k.getItems()) {
								final var col = (DBColumn) m.getData();

								if (col.getConvertedName().equals(column.getConvertedName())) {
									view.getTree().select(m);
									return;
								}
							}
						}
					}
				}
			}
		}

		// Just expand the project and select the database tree item
		if (database != null && table == null && namespace == null) {
			for (final TreeItem i : projectItem.getItems())
				if (i.getData() instanceof Database) {
					view.getTree().select(i);
					return;
				}
		}

		// Check if the last selected item represents either a project, a form group or an integration container!
		if (itemText != null) {
			if (itemText.equals(projectItem.getText())) {
				view.getTree().select(projectItem);
				return;
			}

			for (final TreeItem i : projectItem.getItems())
				if (i.getText().equals(itemText)) {
					view.getTree().select(i);
					return;
				}
		}

		// Expand the form group
		for (final TreeItem i : projectItem.getItems()) {
			if (!i.getText().equals(ProjectView.FORM_GROUPS_LABEL))
				continue;

			// If the user selected the tree item "Form groups" we won't need further analysis!
			if (group == null) {
				view.getTree().select(i);
				return;
			}

			// Expand the root node of all form groups!
			i.setExpanded(true);

			// We save the tree path in a linked list
			final LinkedList<TreeItem> groupList = new LinkedList<>();
			final TreeItem item = getFormGroupsToBeExpanded(i, groupList);
			groupList.addFirst(item);

			final ListIterator<TreeItem> iterator = groupList.listIterator();

			// We iterate from top to bottom
			while (iterator.hasNext()) {
				final TreeItem g = iterator.next();

				if (g == null)
					continue;

				if (!g.equals(groupList.getLast()) || (g.equals(groupList.getLast()) && (form != null || formPanel != null)))
					g.setExpanded(true);

				if (form == null && formPanel == null)
					view.getTree().select(g);

				if (form != null && formPanel == null) {
					for (final TreeItem k : g.getItems()) {
						if (!(k.getData() instanceof final Form aForm))
							continue;

						if (aForm.getName().equals(form.getName())) {
							view.getTree().select(k);
							return;
						}
					}
				}

				if (form == null && formPanel != null) {
					for (final TreeItem k : g.getItems()) {
						if (!(k.getData() instanceof final FormPanel aFormPanel))
							continue;

						if (aFormPanel.getName().equals(formPanel.getName())) {
							view.getTree().select(k);
							return;
						}
					}
				}
			}
		}
	}

}
