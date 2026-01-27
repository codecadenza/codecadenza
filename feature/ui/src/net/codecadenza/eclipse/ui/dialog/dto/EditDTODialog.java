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
package net.codecadenza.eclipse.ui.dialog.dto;

import static net.codecadenza.eclipse.shared.Constants.DTO_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.IMG_ATTRIBUTE;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.dto.DtoFactory;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.dto.DTOBeanService;
import net.codecadenza.eclipse.service.mapping.MappingObjectService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.util.DomainObjectProposalTextField;
import net.codecadenza.eclipse.ui.panel.DomainObjectTreePanel;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Dialog for creating and maintaining data transfer objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditDTODialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE_CREATE = "Create new data transfer object";
	private static final String DLG_TITLE_EDIT = "Edit data transfer object";
	private static final String DEFAULT_ALIAS = "a";

	private final Project project;
	private final DTOBeanService dtoService;
	private String title = DLG_TITLE_CREATE;
	private Tree treeDTOAttributes;
	private DomainObjectTreePanel domainObjectTreePanel;
	private Text txtDTOName;
	private Text txtDomainObjectName;
	private DomainObjectProposalTextField propDomainObject;
	private Button chkStandardConv;
	private DTOBean dtoBean;
	private Menu mnuDTOAttribute;
	private TreeItem itemDTO;
	private DomainAttribute selectedDomainAttribute;
	private AbstractDomainAssociation selectedDomainAssoc;
	private boolean editMode;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 */
	public EditDTODialog(Shell parentShell, Project project) {
		super(parentShell);

		this.project = project;
		this.dtoService = new DTOBeanService(project);
	}

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param dtoBean
	 */
	public EditDTODialog(Shell parentShell, DTOBean dtoBean) {
		this(parentShell, dtoBean.getNamespace().getProject());

		this.dtoBean = dtoBean;
		this.editMode = true;
		this.title = DLG_TITLE_EDIT;
	}

	/**
	 * Initialize dialog components and create a default DTO for the selected domain object
	 * @param domainObject the root domain object
	 */
	private void initDialog(DomainObject domainObject) {
		try {
			treeDTOAttributes.removeAll();

			domainObjectTreePanel.setStandardConversion(chkStandardConv.getSelection());
			domainObjectTreePanel.init(domainObject);

			txtDTOName.setText(domainObject.getName() + DTO_SUFFIX);

			dtoBean = DtoFactory.eINSTANCE.createDTOBean();
			dtoBean.setName(txtDTOName.getText());
			dtoBean.setDomainObject(domainObject);
			dtoBean.setStandardConversion(chkStandardConv.getSelection());
			dtoBean.setCreatedManually(true);

			itemDTO = new TreeItem(treeDTOAttributes, SWT.NONE);
			itemDTO.setText(txtDTOName.getText());
			itemDTO.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_CLASS));
			itemDTO.setData(dtoBean);

			// Add attributes to the DTO. Attributes of domain associations must be added manually as addDTOAttribute() cannot determine
			// the association attribute if the parameter 'selectedTreeItem' is null!
			domainObject.getAllAttributes().forEach(attr -> addDTOAttribute(attr, null, null));

			itemDTO.setExpanded(true);
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
		}
	}

	/**
	 * Initialize dialog components
	 */
	private void initDialog() {
		final DomainObject domainObject = dtoBean.getDomainObject();

		domainObjectTreePanel.setStandardConversion(dtoBean.isStandardConversion());
		domainObjectTreePanel.init(domainObject);

		txtDTOName.setText(dtoBean.getName());
		txtDomainObjectName.setText(dtoBean.getDomainObject().getName());

		itemDTO = new TreeItem(treeDTOAttributes, SWT.NONE);
		itemDTO.setText(dtoBean.getName());
		itemDTO.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_CLASS));
		itemDTO.setData(dtoBean);

		// Add all existing DTO attributes to the tree
		dtoBean.getAttributes().forEach(this::addDTOAttributeToTree);

		itemDTO.setExpanded(true);
	}

	/**
	 * Add a DTO bean attribute
	 * @param domainAttribute
	 * @param domainAssoc
	 * @param selectedTreeItem
	 */
	private void addDTOAttribute(DomainAttribute domainAttribute, AbstractDomainAssociation domainAssoc,
			TreeItem selectedTreeItem) {
		final DTOBeanAttribute dtoAttribute;
		final List<AbstractDomainAssociation> assocList = domainObjectTreePanel.getAssociationListOfSelectedTreeItem();
		final var tokenMap = new HashMap<Integer, String>();
		var dtoAttrName = "";
		var selectToken = "";
		int tokenHierarchyCounter = 1;

		if (domainAttribute != null) {
			// Avoid adding DTO attributes that are mapped to transient fields, byte arrays, or element collections if the DTO is
			// intended for search operations
			if (!dtoBean.isStandardConversion() && (!domainAttribute.isPersistent() || domainAttribute.isLob()
					|| domainAttribute.getCollectionType() != CollectionTypeEnumeration.NONE))
				return;

			dtoAttrName = domainAttribute.getName();

			if (domainAssoc != null) {
				dtoAttrName = domainAssoc.getName() + domainAttribute.getUpperCaseName();

				TreeItem parentItem = selectedTreeItem != null ? selectedTreeItem.getParentItem() : null;

				while (true) {
					if (parentItem != null && parentItem.getParentItem() != null
							&& parentItem.getParentItem().getData() instanceof final AbstractDomainAssociation assoc) {
						dtoAttrName = dtoAttrName.substring(0, 1).toUpperCase() + dtoAttrName.substring(1);
						dtoAttrName = assoc.getName() + dtoAttrName;

						tokenMap.put(++tokenHierarchyCounter, "." + assoc.getName());
					}
					else
						break;

					parentItem = parentItem.getParentItem();
				}

				for (final int tokenId : tokenMap.keySet().stream().sorted((token1, token2) -> token2 - token1).toList())
					selectToken += tokenMap.get(tokenId);

				if (selectToken.isEmpty())
					selectToken = DEFAULT_ALIAS;
				else
					selectToken = DEFAULT_ALIAS + selectToken;

				selectToken += "." + domainAssoc.getName() + "." + domainAttribute.getName();
			}
			else
				selectToken = DEFAULT_ALIAS + "." + domainAttribute.getName();

			dtoAttribute = dtoBean.addAttribute(domainAttribute, dtoAttrName, assocList, false);
		}
		else {
			// If the selected item represents a domain association the respective attribute must be mapped to a DTO!
			final var dlg = new DTOBeanSelectDialog(getShell(), domainAssoc.getTarget());

			if (dlg.open() != Dialog.OK)
				return;

			dtoAttrName = domainAssoc.getName();
			TreeItem parentItem = selectedTreeItem != null ? selectedTreeItem.getParentItem() : null;

			while (true) {
				if (parentItem != null && parentItem.getData() instanceof final AbstractDomainAssociation assoc) {
					dtoAttrName = dtoAttrName.substring(0, 1).toUpperCase() + dtoAttrName.substring(1);
					dtoAttrName = assoc.getName() + dtoAttrName;
				}
				else
					break;

				parentItem = parentItem.getParentItem();
			}

			dtoAttribute = dtoBean.addAttribute(dlg.getDtoBean(), dtoAttrName, assocList, false);
		}

		if (!dtoBean.isStandardConversion())
			dtoAttribute.setSelectToken(selectToken);

		// Add an attribute to the tree
		addDTOAttributeToTree(dtoAttribute);
	}

	/**
	 * Add a DTO attribute to the tree
	 * @param dtoAttribute
	 */
	private void addDTOAttributeToTree(DTOBeanAttribute dtoAttribute) {
		final var itemAttr = new TreeItem(itemDTO, SWT.NONE);
		itemAttr.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
		itemAttr.setData(dtoAttribute);
		itemAttr.setText(generateDTOAttributeText(dtoAttribute));
	}

	/**
	 * Generate the text for a tree item that represents a DTO attribute
	 * @param dtoAttribute
	 * @return the generated item text
	 */
	private String generateDTOAttributeText(DTOBeanAttribute dtoAttribute) {
		if (dtoAttribute.getReferencedDTOBean() != null) {
			if (dtoAttribute.getAssociation() instanceof ManyToManyAssociation
					|| dtoAttribute.getAssociation() instanceof OneToManyAssociation)
				return JavaTypeModifierEnumeration.COLLECTION.toString() + "<" + dtoAttribute.getReferencedDTOBean().getName() + "> "
						+ dtoAttribute.getName();

			return dtoAttribute.getReferencedDTOBean().getName() + " " + dtoAttribute.getName();
		}

		return dtoAttribute.getDomainAttribute().getTypeName() + " " + dtoAttribute.getName();
	}

	/**
	 * Delete the selected DTO attribute
	 */
	private void deleteDTOAttribute() {
		final TreeItem selItem = getSelectedItem();

		if (selItem == null || selItem.getData() == null || !(selItem.getData() instanceof final DTOBeanAttribute dtoAttribute))
			return;

		try {
			dtoService.deleteDTOAttribute(dtoAttribute);
		}
		catch (final IllegalStateException e) {
			MessageDialog.openInformation(getShell(), title, e.getMessage());
			return;
		}

		// Dispose the selected item
		selItem.dispose();
	}

	/**
	 * Rename the selected DTO attribute
	 */
	private void renameDTOAttribute() {
		final var dlgTitle = "Rename DTO attribute";
		final TreeItem selItem = getSelectedItem();

		if (selItem == null || selItem.getData() == null || !(selItem.getData() instanceof final DTOBeanAttribute dtoAttribute))
			return;

		final var dlg = new InputDialog(getShell(), dlgTitle, "Enter a new attribute name:", dtoAttribute.getName(), newName -> {
			final DTOBean dto = dtoAttribute.getDTOBean();
			final IStatus status = EclipseIDEService.validateFieldName(newName);

			if (status.getSeverity() > IStatus.INFO)
				return status.getMessage();

			return dto.getAttributes().stream().filter(attr -> !attr.equals(dtoAttribute) && attr.getName().equals(newName)).findFirst()
					.map(_ -> "A DTO attribute with the same name already exists!").orElse(null);
		});

		if (dlg.open() != Window.OK)
			return;

		dtoAttribute.setName(dlg.getValue());
		selItem.setText(generateDTOAttributeText(dtoAttribute));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var groupBasicData = new Group(panDialogArea, SWT.NONE);
		groupBasicData.setText("Basic data");
		groupBasicData.setLayout(new GridLayout(4, false));
		groupBasicData.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

		final var lblDomainObj = new Label(groupBasicData, SWT.NONE);
		lblDomainObj.setText("Domain object:");

		if (!editMode) {
			propDomainObject = new DomainObjectProposalTextField(groupBasicData, project) {
				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#onProposalAccepted(java.lang.Object)
				 */
				@Override
				public void onProposalAccepted(DomainObject domainObject) {
					initDialog(domainObject);
				}
			};

			propDomainObject.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		}
		else {
			txtDomainObjectName = new Text(groupBasicData, SWT.BORDER | SWT.READ_ONLY);
			txtDomainObjectName.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));
			txtDomainObjectName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		}

		final var lblBeanName = new Label(groupBasicData, SWT.NONE);
		lblBeanName.setText("Bean name:");

		if (!editMode) {
			txtDTOName = new Text(groupBasicData, SWT.BORDER);
			txtDTOName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			txtDTOName.addKeyListener(new KeyAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
				 */
				@Override
				public void keyReleased(KeyEvent e) {
					if (itemDTO != null)
						itemDTO.setText(txtDTOName.getText());
				}
			});
		}
		else {
			txtDTOName = new Text(groupBasicData, SWT.BORDER | SWT.READ_ONLY);
			txtDTOName.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));
			txtDTOName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			final var lblCreatedManually = new Label(groupBasicData, SWT.NONE);
			lblCreatedManually.setText("Created manually:");

			final var chkCreatedManually = new Button(groupBasicData, SWT.CHECK);
			chkCreatedManually.setSelection(dtoBean.isCreatedManually());
			chkCreatedManually.setEnabled(false);
		}

		final var lblStandardConv = new Label(groupBasicData, SWT.NONE);
		lblStandardConv.setText("Standard conversion:");

		chkStandardConv = new Button(groupBasicData, SWT.CHECK);
		chkStandardConv.setSelection(true);

		if (!editMode) {
			chkStandardConv.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (propDomainObject.getSelectedItem() != null)
						initDialog(propDomainObject.getSelectedItem());
				}
			});
		}
		else {
			chkStandardConv.setSelection(dtoBean.isStandardConversion());
			chkStandardConv.setEnabled(false);
		}

		final var sashForm = new SashForm(panDialogArea, SWT.NONE);
		sashForm.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var panDomainObj = new Composite(sashForm, SWT.NONE);
		panDomainObj.setLayout(new GridLayout());

		final var lblDomain = new Label(panDomainObj, SWT.NONE);
		lblDomain.setFont(JFaceResources.getBannerFont());
		lblDomain.setText("Domain object hierarchy");

		final var gdDomainObjectTreePanel = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdDomainObjectTreePanel.widthHint = 400;
		gdDomainObjectTreePanel.heightHint = 400;

		domainObjectTreePanel = new DomainObjectTreePanel(panDomainObj) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.eclipse.ui.dialog.util.DomainObjectTreePanel#addDomainAssociation(net.codecadenza.eclipse.model.
			 * domain.AbstractDomainAssociation)
			 */
			@Override
			protected boolean addDomainAssociation(AbstractDomainAssociation assoc) {
				final DomainObject rootDomainObject = editMode ? dtoBean.getDomainObject() : propDomainObject.getSelectedItem();

				if (chkStandardConv.getSelection() && assoc.getDomainObject().equals(rootDomainObject)) {
					// Bidirectional one-to-many associations are not supported here!
					return !(assoc instanceof final OneToManyAssociation otm && otm.isBidirectional());
				}

				return assoc instanceof OneToOneAssociation || assoc instanceof ManyToOneAssociation;
			}
		};

		domainObjectTreePanel.setLayoutData(gdDomainObjectTreePanel);

		final var panAttributeSelection = new Composite(sashForm, SWT.NONE);
		panAttributeSelection.setLayout(new GridLayout());

		final var lblDTOAttributes = new Label(panAttributeSelection, SWT.NONE);
		lblDTOAttributes.setFont(JFaceResources.getBannerFont());
		lblDTOAttributes.setText("Selected attributes");

		treeDTOAttributes = new Tree(panAttributeSelection, SWT.BORDER);
		treeDTOAttributes.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		treeDTOAttributes.addMenuDetectListener(_ -> {
			treeDTOAttributes.setMenu(null);

			final TreeItem selItem = getSelectedItem();

			if (selItem == null || selItem.getData() == null || !(selItem.getData() instanceof DTOBeanAttribute))
				return;

			treeDTOAttributes.setMenu(mnuDTOAttribute);
		});

		sashForm.setWeights(1, 1);

		mnuDTOAttribute = new Menu(treeDTOAttributes);

		final var mniDelete = new MenuItem(mnuDTOAttribute, SWT.NONE);
		mniDelete.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_DELETE));
		mniDelete.setText("Delete");

		mniDelete.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				deleteDTOAttribute();
			}
		});

		final var mniRename = new MenuItem(mnuDTOAttribute, SWT.NONE);
		mniRename.setText("Rename");

		mniRename.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				renameDTOAttribute();
			}
		});

		// Allow data to be copied or moved to the drop target
		final int operations = DND.DROP_MOVE | DND.DROP_COPY | DND.DROP_DEFAULT;
		final var target = new DropTarget(treeDTOAttributes, operations);

		// Receive the data in text format
		final TextTransfer textTransfer = TextTransfer.getInstance();
		final var types = new Transfer[] { textTransfer };

		target.setTransfer(types);

		target.addDropListener(new DropTargetAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#dragEnter(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dragEnter(DropTargetEvent event) {
				event.detail = DND.DROP_COPY;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#dropAccept(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dropAccept(DropTargetEvent event) {
				try {
					final TreeItem itemDrop = treeDTOAttributes.getItem(treeDTOAttributes.toControl(new Point(event.x, event.y)));

					if (itemDrop != null && itemDrop.getData() != null && itemDrop.getData() instanceof DTOBean) {
						event.detail = DND.DROP_COPY;
						return;
					}

					event.detail = DND.DROP_NONE;
				}
				catch (final Exception e) {
					CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				}
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#drop(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void drop(DropTargetEvent event) {
				try {
					final TreeItem itemDrop = treeDTOAttributes.getItem(treeDTOAttributes.toControl(new Point(event.x, event.y)));
					final TreeItem itemDrag = domainObjectTreePanel.getSelectedItem();

					if (itemDrop == null || itemDrag == null || itemDrop.getData() == null || itemDrag.getData() == null)
						return;

					if (!(itemDrop.getData() instanceof DTOBean))
						return;

					selectedDomainAttribute = null;
					selectedDomainAssoc = null;

					if (itemDrag.getData() instanceof final DomainAttribute domainAttribute) {
						selectedDomainAttribute = domainAttribute;

						if (itemDrag.getParentItem().getData() instanceof final AbstractDomainAssociation assoc)
							selectedDomainAssoc = assoc;
					}
					else if (itemDrag.getData() instanceof final AbstractDomainAssociation assoc)
						selectedDomainAssoc = assoc;

					addDTOAttribute(selectedDomainAttribute, selectedDomainAssoc, itemDrag);
				}
				catch (final IllegalStateException e) {
					MessageDialog.openInformation(getShell(), title, e.getMessage());
				}
				catch (final Exception e) {
					CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				}
			}
		});

		if (editMode)
			initDialog();
		else
			propDomainObject.getControl().setFocus();

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(title);
	}

	/**
	 * Apply input to the new data transfer object
	 * @throws IllegalStateException if the namespace for the data transfer object could not be found
	 */
	private void applyInput() {
		final DomainObject domainObject = dtoBean.getDomainObject();
		final Stream<Namespace> nsStream = project.getDTONamespace().getChildNamespaces().stream();
		final Optional<Namespace> dtoNamespace = nsStream.filter(e -> e.getName().equals(domainObject.getNamespace().getName()))
				.findFirst();

		if (!dtoNamespace.isPresent())
			throw new IllegalStateException("The DTO namespace could not be found!");

		dtoBean.setComment("Instances of this class represent data transfer objects that are mapped to " + domainObject.getLabel()
				+ " domain objects");
		dtoBean.setNamespace(dtoNamespace.orElse(null));
		dtoBean.setMappable(false);
		dtoBean.setPrimitive(false);
		dtoBean.getNamespace().getJavaTypes().add(dtoBean);

		project.eResource().getContents().add(dtoBean);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			try {
				if (!editMode) {
					// Validate the user input
					final DomainObject selectedDomainObj = propDomainObject.getSelectedItem();

					if (selectedDomainObj == null) {
						MessageDialog.openInformation(getShell(), title, "A domain object must be selected!");
						propDomainObject.getControl().setFocus();
						return;
					}

					// The name of the DTO must be set prior to perform the validation!
					dtoBean.setName(txtDTOName.getText());

					final IStatus status = new MappingObjectService(project).validateMappingObject(dtoBean);

					if (!status.isOK())
						throw new IllegalStateException(status.getMessage());

					applyInput();
				}

				EclipseIDEService.saveProjectMetaData(project);

				new DTOBeanService(project).rebuildDTOBeanSourceFiles(dtoBean);

				if (editMode) {
					// Search for the corresponding boundary and rebuild it if the DTO already exists!
					final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(dtoBean.getDomainObject());

					if (boundaryBean != null)
						new BoundaryService(project).rebuildBoundarySourceFiles(boundaryBean);
				}
			}
			catch (final Exception e) {
				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
				return;
			}
		}

		super.buttonPressed(buttonId);
	}

	/**
	 * @return the selected item in the DTO tree view or null if no item is selected
	 */
	private TreeItem getSelectedItem() {
		return Arrays.asList(treeDTOAttributes.getSelection()).stream().findFirst().orElse(null);
	}

}
