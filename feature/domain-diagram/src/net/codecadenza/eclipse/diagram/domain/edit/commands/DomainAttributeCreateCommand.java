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
package net.codecadenza.eclipse.diagram.domain.edit.commands;

import static net.codecadenza.eclipse.shared.Constants.DB_PRIMARY_KEY_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.DB_UNIQUE_KEY_PREFIX;

import net.codecadenza.eclipse.diagram.domain.dialog.NewDomainAttributeDialog;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.db.PrimaryKey;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.CollectionMappingStrategyEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainInheritance;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand;
import org.eclipse.gmf.runtime.emf.type.core.requests.CreateElementRequest;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Command to create a domain attribute
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainAttributeCreateCommand extends CreateElementCommand {
	private static final String DLG_TITLE = "Create new domain attribute";

	/**
	 * @param req
	 */
	public DomainAttributeCreateCommand(CreateElementRequest req) {
		super(req);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#getElementToEdit()
	 */
	@Override
	protected EObject getElementToEdit() {
		EObject container = ((CreateElementRequest) getRequest()).getContainer();

		if (container instanceof final View view)
			container = view.getElement();

		return container;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#getEClassToEdit()
	 */
	@Override
	protected EClass getEClassToEdit() {
		return DomainPackage.eINSTANCE.getDomainObject();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.emf.type.core.commands.CreateElementCommand#doDefaultElementCreation()
	 */
	@Override
	protected EObject doDefaultElementCreation() {
		DomainAttribute attribute = (DomainAttribute) super.doDefaultElementCreation();
		final Project project = attribute.getDomainObject().getNamespace().getProject();
		final Shell shell = Display.getCurrent().getActiveShell();
		final var dlg = new NewDomainAttributeDialog(shell, attribute, project);

		if (dlg.open() != Dialog.OK)
			throw new IllegalStateException();

		attribute = dlg.getDomainAttribute();

		try {
			if (attribute.getCollectionMappingStrategy() != CollectionMappingStrategyEnumeration.TABLE)
				initAttribute(attribute, dlg.isUnique());
			else
				initCollectionTable(attribute, dlg.getCollectionTableName(), dlg.isUnique());

			attribute.getDomainObject().getAttributes().add(attribute);
		}
		catch (final Exception e) {
			CodeCadenzaDiagramEditorPlugin.getInstance().handleInternalError(e);

			throw new IllegalStateException(e);
		}

		try {
			final var domainObjectService = new DomainObjectService(project);
			domainObjectService.rebuildDomainObjectSourceFiles(attribute.getDomainObject(), false);
		}
		catch (final Exception e) {
			CodeCadenzaDiagramEditorPlugin.getInstance().handleInternalError(e);
		}

		return attribute;
	}

	/**
	 * @param attr
	 * @param addUniqueKey
	 */
	private void initAttribute(DomainAttribute attr, boolean addUniqueKey) {
		final Shell shell = Display.getCurrent().getActiveShell();

		// Check if the attribute may be added!
		for (final DomainAttribute a : attr.getDomainObject().getAllAttributes()) {
			if (a.equals(attr))
				continue;

			if (attr.isPk() && a.isPk())
				throw new IllegalStateException("A primary key attribute ('" + a.getName() + "') already exists!");

			if (attr.isDisplayAttribute() && a.isDisplayAttribute())
				throw new IllegalStateException("A display attribute ('" + a.getName() + "') already exists!");

			if (a.getName().equals(attr.getName()))
				throw new IllegalStateException("An attribute with the same name already exists!");
		}

		for (final AbstractDomainAssociation assoc : attr.getDomainObject().getAllAssociations())
			if (assoc.getName().equals(attr.getName()))
				throw new IllegalStateException("An association with the same name already exists!");

		if (attr.isPersistent()) {
			DBTable table = attr.getDomainObject().getDatabaseTable();

			if (table == null && attr.getDomainObject().getParent() != null)
				table = attr.getDomainObject().getRootParentDomainObject(false).getDatabaseTable();

			// Set the proper database table
			attr.getColumn().setDatabaseTable(table);

			if (attr.getDomainObject().getParent() != null
					&& !attr.getDomainObject().equals(attr.getDomainObject().getRootParentDomainObject(false))
					&& attr.getDomainObject().getInheritanceType() == InheritanceTypeEnumeration.SINGLE_TABLE)
				attr.getColumn().setNullable(true);

			// Add a primary key if necessary
			if (attr.isPk()) {
				final PrimaryKey pk = DbFactory.eINSTANCE.createPrimaryKey();
				pk.setColumn(attr.getColumn());
				pk.setTable(table);
				pk.setName(DB_PRIMARY_KEY_PREFIX + table.getName());

				table.setPrimaryKey(pk);
			}
			else
				attr.setPk(false);

			// Add a unique key if necessary
			if (addUniqueKey) {
				final var keyName = DB_UNIQUE_KEY_PREFIX + table.getShortTableName() + "_" + attr.getColumn().getName();

				final DBIndex index = DbFactory.eINSTANCE.createDBIndex();
				index.setName(keyName);
				index.getColumns().add(attr.getColumn());
				index.setUnique(true);
				index.setTable(table);

				table.getIndexes().add(index);
			}

			if (attr.getTag() == AttributeTagEnumeration.CLIENT_DISPLAY) {
				// If the domain object directly references the client domain object a unique key can be added!
				for (final AbstractDomainAssociation assoc : attr.getDomainObject().getAssociations())
					if (assoc instanceof final ManyToOneAssociation mto && assoc.getTag() == AssociationTagEnumeration.CLIENT_REFERENCE) {
						final var keyName = DB_UNIQUE_KEY_PREFIX + table.getShortTableName() + "_" + attr.getColumn().getName() + "_"
								+ mto.getColumn().getName();

						final DBIndex index = DbFactory.eINSTANCE.createDBIndex();
						index.setName(keyName);
						index.getColumns().add(attr.getColumn());
						index.getColumns().add(mto.getColumn());
						index.setUnique(true);
						index.setTable(table);

						table.getIndexes().add(index);
					}
			}

			// Add further columns to all derived beans if the attribute belongs to a mapped superclass
			if (attr.getDomainObject().isMappedSuperClass()) {
				for (final DomainInheritance i : attr.getDomainObject().getTargetInheritances()) {
					final DomainObject bean = i.getSource();

					DBTable thisTable;

					if (bean.getDatabaseTable() != null)
						thisTable = bean.getDatabaseTable();
					else
						thisTable = bean.getRootParentDomainObject(false).getDatabaseTable();

					// Check if a derived class has a column with the same name
					for (final DBColumn tmp : thisTable.getColumns()) {
						if (tmp.getConvertedName().equals(attr.getColumn().getConvertedName())) {
							final var msg = "A column with the same name already exists in table '" + bean.getDatabaseTable().getConvertedName()
									+ "'!";

							MessageDialog.openInformation(shell, DLG_TITLE, msg);
							throw new IllegalStateException();
						}
					}

					final DBColumn col = DbFactory.eINSTANCE.createDBColumn();
					col.setColumnType(attr.getColumn().getColumnType());
					col.setLength(attr.getColumn().getLength());
					col.setName(attr.getColumn().getName());
					col.setNullable(attr.getColumn().isNullable());
					col.setPrecision(attr.getColumn().getPrecision());
					col.setScale(attr.getColumn().getScale());
					col.setDatabaseTable(thisTable);
					thisTable.getColumns().add(col);
				}
			}
		}
	}

	/**
	 * @param attr
	 * @param tableName
	 * @param addUniqueKey
	 */
	private void initCollectionTable(DomainAttribute attr, String tableName, boolean addUniqueKey) {
		final DBColumn valueColumn = attr.getColumn();
		DBTable refTable = attr.getDomainObject().getDatabaseTable();

		if (refTable == null && attr.getDomainObject().getParent() != null)
			refTable = attr.getDomainObject().getRootParentDomainObject(false).getDatabaseTable();

		if (refTable == null)
			throw new IllegalStateException("A collection table without the table of the owning domain object cannot be created!");

		final PrimaryKey refPrimaryKey = refTable.getPrimaryKey();

		if (refPrimaryKey == null)
			throw new IllegalStateException(
					"A collection table without a primary key attribute of the owning domain object cannot be created!");

		final DBColumn refPkColumn = refPrimaryKey.getColumn();
		final Database database = refTable.getDatabase();

		final DBTable collectionTable = DbFactory.eINSTANCE.createDBTable();
		collectionTable.setSchemaName(refTable.getSchemaName());
		collectionTable.setCatalogName(refTable.getCatalogName());
		collectionTable.setDatabase(database);
		collectionTable.setName(tableName);

		final DBColumn joinColumn = DbFactory.eINSTANCE.createDBColumn();
		joinColumn.setColumnType(refPkColumn.getColumnType());
		joinColumn.setDatabaseTable(collectionTable);
		joinColumn.setLength(refPkColumn.getLength());
		joinColumn.setName(refPkColumn.getName());
		joinColumn.setNullable(false);
		joinColumn.setPrecision(refPkColumn.getPrecision());
		joinColumn.setScale(refPkColumn.getScale());
		joinColumn.addForeignKey(refPkColumn, addUniqueKey);

		if (addUniqueKey) {
			final var keyName = DB_UNIQUE_KEY_PREFIX + collectionTable.getShortTableName();

			final DBIndex index = DbFactory.eINSTANCE.createDBIndex();
			index.setName(keyName);
			index.getColumns().add(joinColumn);
			index.getColumns().add(valueColumn);
			index.setUnique(true);
			index.setTable(collectionTable);

			collectionTable.getIndexes().add(index);
		}

		collectionTable.getColumns().add(joinColumn);
		collectionTable.getColumns().add(valueColumn);

		final Resource eResource = attr.eResource();
		eResource.getContents().add(collectionTable);

		database.getDatabaseTables().add(collectionTable);
		valueColumn.setDatabaseTable(collectionTable);
	}

}
