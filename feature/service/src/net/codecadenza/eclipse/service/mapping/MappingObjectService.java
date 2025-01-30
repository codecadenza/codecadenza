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
package net.codecadenza.eclipse.service.mapping;

import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.CodeCadenzaServicePlugin;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/**
 * <p>
 * Service for mapping objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MappingObjectService {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public MappingObjectService(Project project) {
		this.project = project;
	}

	/**
	 * @param mappingObject
	 * @return a status object that informs the caller about the validation result
	 */
	public IStatus validateMappingObject(final MappingObject mappingObject) {
		// Check if the name is a valid Java type name
		final IStatus status = EclipseIDEService.validateJavaTypeName(mappingObject.getName());

		if (!status.isOK())
			return status;

		final String namespaceName = mappingObject.getDomainObject().getNamespace().getName();

		// Search for classes that have the same name
		for (final Namespace ns : project.getDTONamespace().getChildNamespaces()) {
			if (!ns.getName().equals(namespaceName))
				continue;

			for (final JavaType type : ns.getJavaTypes()) {
				if (mappingObject.equals(type))
					continue;

				if (!type.getName().equals(mappingObject.getName()))
					continue;

				if (ns.getName().equals(namespaceName)) {
					var messageText = "A class with the name '";
					messageText += type.getName() + "' already exists in the ";

					if (type instanceof final DTOBean dto && dto.isVirtual())
						messageText += "meta-model!";
					else
						messageText += "same package!";

					return new Status(IStatus.ERROR, CodeCadenzaServicePlugin.PLUGIN_ID, IStatus.ERROR, messageText, null);
				}
			}
		}

		return new Status(IStatus.OK, CodeCadenzaServicePlugin.PLUGIN_ID, "");
	}

}
