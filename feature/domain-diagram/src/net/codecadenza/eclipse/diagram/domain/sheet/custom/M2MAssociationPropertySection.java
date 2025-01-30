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
package net.codecadenza.eclipse.diagram.domain.sheet.custom;

import net.codecadenza.eclipse.diagram.domain.edit.parts.ManyToManyAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import org.eclipse.emf.transaction.RecordingCommand;
import org.eclipse.gmf.runtime.notation.Edge;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.tabbed.AbstractPropertySection;
import org.eclipse.ui.views.properties.tabbed.ITabbedPropertyConstants;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;

/**
 * <p>
 * Property section for many-to-many associations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class M2MAssociationPropertySection extends AbstractPropertySection {
	private static final int LABEL_WIDTH = 220;

	private Text txtName;
	private Button chkFetchTypeEager;
	private ManyToManyAssociation association;
	private ManyToManyAssociationEditPart part;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#createControls(org.eclipse.swt.widgets.Composite,
	 * org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage)
	 */
	@Override
	public void createControls(Composite parent, TabbedPropertySheetPage aTabbedPropertySheetPage) {
		super.createControls(parent, aTabbedPropertySheetPage);

		final Composite panPropertySection = getWidgetFactory().createFlatFormComposite(parent);

		var data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(0, ITabbedPropertyConstants.VSPACE);

		txtName = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtName.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtName, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtName, 0, SWT.CENTER);

		final CLabel lblName = getWidgetFactory().createCLabel(panPropertySection, "Name:");
		lblName.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtName, ITabbedPropertyConstants.VSPACE);

		chkFetchTypeEager = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkFetchTypeEager.setLayoutData(data);

		chkFetchTypeEager.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				updateMetaModel();
			}
		});

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkFetchTypeEager, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkFetchTypeEager, 0, SWT.CENTER);

		final CLabel lblFetchType = getWidgetFactory().createCLabel(panPropertySection, "Eager fetch:");
		lblFetchType.setLayoutData(data);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#refresh()
	 */
	@Override
	public void refresh() {
		txtName.setText(association.getName());
		chkFetchTypeEager.setSelection(association.isFetchTypeEager());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#setInput(org.eclipse.ui.IWorkbenchPart,
	 * org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void setInput(IWorkbenchPart part, ISelection selection) {
		super.setInput(part, selection);

		final Object input = ((IStructuredSelection) selection).getFirstElement();
		this.part = (ManyToManyAssociationEditPart) input;
		this.association = (ManyToManyAssociation) ((Edge) ((ManyToManyAssociationEditPart) input).getModel()).getElement();
	}

	/**
	 * Update the meta-model and rebuild the respective domain object source files
	 */
	private void updateMetaModel() {
		part.getEditingDomain().getCommandStack().execute(new RecordingCommand(part.getEditingDomain()) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
			 */
			@Override
			protected void doExecute() {
				association.setFetchTypeEager(chkFetchTypeEager.getSelection());
			}
		});

		// Rebuild the respective domain object source files
		try {
			final var domainObjectService = new DomainObjectService(association.getDomainObject().getNamespace().getProject());
			domainObjectService.rebuildDomainObjectSourceFiles(association.getDomainObject(), false);
			domainObjectService.rebuildDomainObjectSourceFiles(association.getTarget(), false);
		}
		catch (final Exception e) {
			CodeCadenzaDiagramEditorPlugin.getInstance().handleInternalError(e);
		}
	}

}
