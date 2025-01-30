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
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.db.PrimaryKey;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
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
			initAttribute(attribute, dlg.isUnique());
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
	 * @param att
	 * @param addUniqueKey
	 */
	private void initAttribute(DomainAttribute att, boolean addUniqueKey) {
		final Shell shell = Display.getCurrent().getActiveShell();

		// Check if the attribute may be added!
		for (final DomainAttribute a : att.getDomainObject().getAllAttributes()) {
			if (a.equals(att))
				continue;

			if (att.isPk() && a.isPk())
				throw new IllegalStateException("A primary key attribute ('" + a.getName() + "') already exists!");

			if (att.isDisplayAttribute() && a.isDisplayAttribute())
				throw new IllegalStateException("A display attribute ('" + a.getName() + "') already exists!");

			if (a.getName().equals(att.getName()))
				throw new IllegalStateException("An attribute with the same name already exists!");
		}

		for (final AbstractDomainAssociation assoc : att.getDomainObject().getAllAssociations())
			if (assoc.getName().equals(att.getName()))
				throw new IllegalStateException("An association with the same name already exists!");

		if (att.isPersistent()) {
			DBTable table = att.getDomainObject().getDatabaseTable();

			if (table == null && att.getDomainObject().getParent() != null)
				table = att.getDomainObject().getRootParentDomainObject(false).getDatabaseTable();

			// Set the proper database table
			att.getColumn().setDatabaseTable(table);

			if (att.getDomainObject().getParent() != null
					&& !att.getDomainObject().equals(att.getDomainObject().getRootParentDomainObject(false))
					&& att.getDomainObject().getInheritanceType() == InheritanceTypeEnumeration.SINGLE_TABLE)
				att.getColumn().setNullable(true);

			// Add a primary key if necessary
			if (att.isPk()) {
				final PrimaryKey pk = DbFactory.eINSTANCE.createPrimaryKey();
				pk.setColumn(att.getColumn());
				pk.setTable(table);
				pk.setName(DB_PRIMARY_KEY_PREFIX + table.getName());

				table.setPrimaryKey(pk);
			}
			else
				att.setPk(false);

			// Add a unique key if necessary
			if (addUniqueKey) {
				final var keyName = DB_UNIQUE_KEY_PREFIX + table.getShortTableName() + "_" + att.getColumn().getName();

				final DBIndex index = DbFactory.eINSTANCE.createDBIndex();
				index.setName(keyName);
				index.getColumns().add(att.getColumn());
				index.setUnique(true);
				index.setTable(table);

				table.getIndexes().add(index);
			}

			if (att.getTag() == AttributeTagEnumeration.CLIENT_DISPLAY) {
				// If the domain object directly references the client domain object a unique key can be added!
				for (final AbstractDomainAssociation assoc : att.getDomainObject().getAssociations())
					if (assoc instanceof final ManyToOneAssociation mto && assoc.getTag() == AssociationTagEnumeration.CLIENT_REFERENCE) {
						final var keyName = DB_UNIQUE_KEY_PREFIX + table.getShortTableName() + "_" + att.getColumn().getName() + "_"
								+ mto.getColumn().getName();

						final DBIndex index = DbFactory.eINSTANCE.createDBIndex();
						index.setName(keyName);
						index.getColumns().add(att.getColumn());
						index.getColumns().add(mto.getColumn());
						index.setUnique(true);
						index.setTable(table);

						table.getIndexes().add(index);
					}
			}

			// Add further columns to all derived beans if the attribute belongs to a mapped superclass
			if (att.getDomainObject().isMappedSuperClass()) {
				for (final DomainInheritance i : att.getDomainObject().getTargetInheritances()) {
					final DomainObject bean = i.getSource();

					DBTable thisTable;

					if (bean.getDatabaseTable() != null)
						thisTable = bean.getDatabaseTable();
					else
						thisTable = bean.getRootParentDomainObject(false).getDatabaseTable();

					// Check if a derived class has a column with the same name
					for (final DBColumn tmp : thisTable.getColumns()) {
						if (tmp.getConvertedName().equals(att.getColumn().getConvertedName())) {
							final var msg = "A column with the same name already exists in table '" + bean.getDatabaseTable().getConvertedName()
									+ "'!";

							MessageDialog.openInformation(shell, DLG_TITLE, msg);
							throw new IllegalStateException();
						}
					}

					final DBColumn col = DbFactory.eINSTANCE.createDBColumn();
					col.setColumnType(att.getColumn().getColumnType());
					col.setLength(att.getColumn().getLength());
					col.setName(att.getColumn().getName());
					col.setNullable(att.getColumn().isNullable());
					col.setPrecision(att.getColumn().getPrecision());
					col.setScale(att.getColumn().getScale());
					col.setDatabaseTable(thisTable);
					thisTable.getColumns().add(col);
				}
			}
		}

		att.getDomainObject().getAttributes().add(att);
	}

}
