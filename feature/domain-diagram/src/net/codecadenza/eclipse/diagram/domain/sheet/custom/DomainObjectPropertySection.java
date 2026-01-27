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

import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainObjectEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.transaction.RecordingCommand;
import org.eclipse.gmf.runtime.notation.Node;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.tabbed.AbstractPropertySection;
import org.eclipse.ui.views.properties.tabbed.ITabbedPropertyConstants;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;

/**
 * <p>
 * Property sheet for domain objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainObjectPropertySection extends AbstractPropertySection {
	private static final String DLG_TITLE = "Edit domain object";
	private static final int LABEL_WIDTH = 220;

	private Text txtNamePlural;
	private Text txtLabel;
	private Text txtLabelPlural;
	private Text txtComment;
	private Text txtDiscVal;
	private Button cmdAbstract;
	private Button cmdPropertyAccess;
	private DomainObject domainObject;
	private DomainObjectEditPart part;
	private ModifyListener textChangeListener;

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

		txtNamePlural = getWidgetFactory().createText(panPropertySection, "");
		txtNamePlural.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtNamePlural, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtNamePlural, 0, SWT.CENTER);

		final CLabel lblNamePlural = getWidgetFactory().createCLabel(panPropertySection, "Name plural:");
		lblNamePlural.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtNamePlural, ITabbedPropertyConstants.VSPACE);

		txtLabel = getWidgetFactory().createText(panPropertySection, "");
		txtLabel.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtLabel, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtLabel, 0, SWT.CENTER);

		final CLabel lblLabel = getWidgetFactory().createCLabel(panPropertySection, "Label:");
		lblLabel.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtLabel, ITabbedPropertyConstants.VSPACE);

		txtLabelPlural = getWidgetFactory().createText(panPropertySection, "");
		txtLabelPlural.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtLabelPlural, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtLabelPlural, 0, SWT.CENTER);

		final CLabel lblLabelPlural = getWidgetFactory().createCLabel(panPropertySection, "Label plural:");
		lblLabelPlural.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtLabelPlural, ITabbedPropertyConstants.VSPACE);

		txtComment = getWidgetFactory().createText(panPropertySection, "");
		txtComment.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtComment, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtComment, 0, SWT.CENTER);

		final CLabel lblComment = getWidgetFactory().createCLabel(panPropertySection, "Comment:");
		lblComment.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtComment, ITabbedPropertyConstants.VSPACE);

		txtDiscVal = getWidgetFactory().createText(panPropertySection, "");
		txtDiscVal.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtDiscVal, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtDiscVal, 0, SWT.CENTER);

		final CLabel lblDiscVal = getWidgetFactory().createCLabel(panPropertySection, "Discriminator value:");
		lblDiscVal.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtDiscVal, ITabbedPropertyConstants.VSPACE);

		cmdAbstract = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		cmdAbstract.setEnabled(false);
		cmdAbstract.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(cmdAbstract, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(cmdAbstract, 0, SWT.CENTER);

		final CLabel lblAbstract = getWidgetFactory().createCLabel(panPropertySection, "Abstract:");
		lblAbstract.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(cmdAbstract, ITabbedPropertyConstants.VSPACE);

		cmdPropertyAccess = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		cmdPropertyAccess.setEnabled(false);
		cmdPropertyAccess.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(cmdPropertyAccess, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(cmdPropertyAccess, 0, SWT.CENTER);

		final CLabel lblPropertyAccess = getWidgetFactory().createCLabel(panPropertySection, "Property access:");
		lblPropertyAccess.setLayoutData(data);

		textChangeListener = _ -> updateMetaModel();
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
		this.part = (DomainObjectEditPart) input;
		this.domainObject = (DomainObject) ((Node) ((DomainObjectEditPart) input).getModel()).getElement();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#refresh()
	 */
	@Override
	public void refresh() {
		txtNamePlural.removeModifyListener(textChangeListener);
		txtLabel.removeModifyListener(textChangeListener);
		txtLabelPlural.removeModifyListener(textChangeListener);
		txtComment.removeModifyListener(textChangeListener);
		txtDiscVal.removeModifyListener(textChangeListener);

		txtNamePlural.setText(domainObject.getNamePlural());
		txtLabel.setText(domainObject.getLabel());
		txtLabelPlural.setText(domainObject.getLabelPlural());
		txtComment.setText(domainObject.getComment());
		txtDiscVal.setText(domainObject.getDiscriminatorValue());
		cmdAbstract.setSelection(domainObject.isAbstract());
		cmdPropertyAccess.setSelection(domainObject.isPropertyAccess());

		txtNamePlural.addModifyListener(textChangeListener);
		txtLabel.addModifyListener(textChangeListener);
		txtLabelPlural.addModifyListener(textChangeListener);
		txtComment.addModifyListener(textChangeListener);
		txtDiscVal.addModifyListener(textChangeListener);
	}

	/**
	 * Validate the input, change the meta-model and rebuild the respective domain object source files
	 */
	private void updateMetaModel() {
		final Shell shell = Display.getCurrent().getActiveShell();
		String validationMessage = null;

		if (txtLabel.getText().isEmpty())
			validationMessage = "The label must not be empty!";

		if (txtLabelPlural.getText().isEmpty())
			validationMessage = "The plural form of the label must not be empty!";

		final IStatus status = EclipseIDEService.validateJavaTypeName(txtNamePlural.getText());

		if (status.getSeverity() > IStatus.INFO)
			validationMessage = status.getMessage();

		if (validationMessage != null) {
			MessageDialog.openInformation(shell, DLG_TITLE, validationMessage);
			refresh();
			return;
		}

		// Save the changes
		part.getEditingDomain().getCommandStack().execute(new RecordingCommand(part.getEditingDomain()) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
			 */
			@Override
			protected void doExecute() {
				domainObject.setNamePlural(txtNamePlural.getText());
				domainObject.setLabel(txtLabel.getText());
				domainObject.setLabelPlural(txtLabelPlural.getText());
				domainObject.setComment(txtComment.getText());
				domainObject.setDiscriminatorValue(txtDiscVal.getText());
			}
		});

		// Rebuild the respective domain object source files
		try {
			final var domainObjectService = new DomainObjectService(domainObject.getNamespace().getProject());
			domainObjectService.rebuildDomainObjectSourceFiles(domainObject, false);
		}
		catch (final Exception e) {
			CodeCadenzaDiagramEditorPlugin.getInstance().handleInternalError(e);
		}
	}

}
