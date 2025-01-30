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
package net.codecadenza.eclipse.generator.client.common.form;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Abstract base class for all single-record form generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractSingleRecordFormGenerator extends AbstractJavaSourceGenerator {
	public static final String INIT_MODEL_OBJ_NAME_PREFIX = "initial";

	protected final Form form;
	protected final Project project;
	protected final DTOBean dto;
	protected final String modelObjectName;
	protected final FormTypeEnumeration formType;
	protected final EList<DTOBean> additionalDTOs;

	/**
	 * Constructor
	 * @param form
	 */
	protected AbstractSingleRecordFormGenerator(Form form) {
		super(form.getSourceFile());

		this.form = form;
		this.formType = form.getFormType();
		this.project = form.getDTO().getNamespace().getProject();
		this.dto = form.getDTO();
		this.modelObjectName = dto.getDomainObject().getLowerCaseName();
		this.additionalDTOs = getAdditionalDTOs();
	}

	/**
	 * @return the generated content
	 */
	protected String addInitializationOfAdditionalFields() {
		final var b = new StringBuilder();

		additionalDTOs.forEach(addDTO -> {
			final String objectName = INIT_MODEL_OBJ_NAME_PREFIX + addDTO.getDomainObject().getName();

			b.append(objectName + " = new " + addDTO.getModelClassName() + "();\n");
		});

		if (!project.isBoundaryMode() && (formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE)) {
			// When working with persistent domain objects all one-to-one associations must be initialized!
			final var oneToOneSet = new HashSet<OneToOneAssociation>();

			for (final DTOBeanAttribute attr : dto.getAttributes())
				if (attr.getAssociation() instanceof final OneToOneAssociation oto && !oneToOneSet.contains(oto)) {
					b.append(modelObjectName + "." + oto.getSetterName() + "(new " + oto.getTarget().getName() + "());\n");

					oneToOneSet.add(oto);
				}
		}

		if (b.isEmpty())
			return b.toString();

		return b.toString() + "\n";
	}

	/**
	 * @return an array that contains all parameters for saving a domain object and its initial one-to-many objects
	 */
	protected String[] getSaveInvocationParameters() {
		final List<String> addParams = additionalDTOs.stream().map(d -> INIT_MODEL_OBJ_NAME_PREFIX + d.getDomainObject().getName())
				.toList();

		final var paramList = new ArrayList<String>();
		paramList.add(dto.getDomainObject().getLowerCaseName());
		paramList.addAll(addParams);

		return paramList.stream().toArray(String[]::new);
	}

	/**
	 * @return a list of DTOs for all initial one-to-many objects
	 */
	private EList<DTOBean> getAdditionalDTOs() {
		final var dtoList = new BasicEList<DTOBean>();

		if (formType != FormTypeEnumeration.ADD && formType != FormTypeEnumeration.CREATE)
			return dtoList;

		// We assume that there is only one action of type 'CREATE'!
		final BoundaryMethod m = form.getActions().stream().filter(a -> a.getType() == ActionType.CREATE).findFirst()
				.map(FormAction::getBoundaryMethod).orElse(null);

		if (m == null) {
			final var typeName = dto.getDomainObject().getName();
			final var msg = "The boundary method for saving a domain object of type '" + typeName + "' could not be found!";

			throw new IllegalStateException(msg);
		}

		for (final MethodParameter p : m.getMethodParameters())
			if (!p.getType().equals(dto) && p.getType() instanceof final DTOBean initDTO)
				dtoList.add(initDTO);

		return dtoList;
	}

}
