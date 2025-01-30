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
package net.codecadenza.eclipse.generator.boundary.method;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;

/**
 * <p>
 * Factory for boundary method generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BoundaryMethodGeneratorFactory {
	/**
	 * Private constructor
	 */
	private BoundaryMethodGeneratorFactory() {

	}

	/**
	 * @param method
	 * @param parentGenerator
	 * @return a generator for the given method
	 */
	public static BasicBoundaryMethodGenerator getMethodGenerator(BoundaryMethod method,
			AbstractJavaSourceGenerator parentGenerator) {
		final var utility = new BoundaryMethodGeneratorUtility(method);

		if (method.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER)
			return new SearchByFilterBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH)
			return new SearchBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.UPDATE)
			return new UpdateBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_BY_ID)
			return new FindByIdBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_BY_OBJECT)
			return new FindByObjectBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_EXISTING)
			return new FindExistingBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.EXISTS_BY_ID)
			return new ExistsByIdBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_BY_PARENT)
			return new FindByParentBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.GET_LIST_OF_VALUES)
			return new GetListOfValuesBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.COUNT)
			return new CountBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.COUNT_ALL)
			return new CountAllBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.CREATE)
			return new CreateBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_ALL)
			return new FindAllBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.CHANGE_PARENT)
			return new ChangeParentBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.LOG_ON)
			return new LogOnBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.DOWNLOAD)
			return new DownloadBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.UPLOAD)
			return new UploadBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.ADD_TO_ASSOCIATION)
			return new AddToAssociationBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION)
			return new RemoveFromAssociationBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.DELETE)
			return new DeleteBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH_BY_UNIQUE_KEY)
			return new SearchByUniqueKeyBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_BY_UNIQUE_KEY)
			return new FindByUniqueKeyBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.GET_ASSOCIATION)
			return new GetAssociationBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY
				|| method.getMethodType() == BoundaryMethodTypeEnumeration.EXISTS_BY_UNIQUE_KEY_WITH_ID)
			return new ExistsByUniqueKeyBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.CHANGE_PASSWORD)
			return new ChangePasswordBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.COPY)
			return new CopyBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.DOWNLOAD_EXPORT)
			return new ExportBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.UPLOAD_IMPORT)
			return new ImportBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.CHANGE_ASSOCIATION)
			return new ChangeAssociationBoundaryMethodGenerator(method, utility, parentGenerator);
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.SAVE)
			return new SaveBoundaryMethodGenerator(method, utility, parentGenerator);

		return new BasicBoundaryMethodGenerator(method, utility, parentGenerator);
	}

}
